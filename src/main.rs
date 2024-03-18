use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use dashmap::DashMap;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug, Clone)]
enum Addr {
    Num(usize),
    Last,
    Ctx {
        re: String,
        delim: char,
        valid: bool,
    },
}

impl Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Addr::Num(n) => write!(f, "{}", n),
            Addr::Last => write!(f, "$"),
            Addr::Ctx {
                re,
                delim: '/',
                valid: _,
            } => write!(f, "/{}/", re),
            Addr::Ctx {
                re,
                delim,
                valid: _,
            } => write!(f, "\\{}{}{}", delim, re, delim),
        }
    }
}

type Address = Option<(Addr, Option<Addr>)>;

#[derive(Debug, Clone)]
enum Command {
    Block(Vec<Cmd>),

    Insert(Vec<String>),
    Append(Vec<String>),
    Change(Vec<String>),

    TwoAddrSimple(char), // dDgGhHlnNpPx
    Quit,
    CurLine,

    Label(String),
    Branch(String),
    Test(String),

    Read(String),
    Write(String),

    Sub {
        re: String,
        delim: char,
        subs: String,
        flags: Vec<String>,
        file: Option<String>,
    },

    Replace {
        delim: char,
        from: String,
        to: String,
    },

    Silent(String),
    Comment(String),
}

#[derive(Debug, Clone)]
struct Cmd {
    a: Address,
    neg: bool,
    c: Command,
    range: Range,
    valid: bool,
}

fn valid_addr(addr: &Addr) -> bool {
    match &addr {
        Addr::Num(n) => *n > 0,
        Addr::Last => true,
        Addr::Ctx {
            re: _,
            delim: _,
            valid,
        } => *valid,
    }
}

fn valid_address(addr: &Address, n: u32) -> bool {
    match addr {
        Some((one, Some(two))) => n >= 2 && valid_addr(one) && valid_addr(two),
        Some((one, None)) => n >= 1 && valid_addr(one),
        None => true,
    }
}

fn fmt_address(addr: &Address) -> String {
    match addr {
        Some((one, Some(two))) => format!("{}, {} ", one, two),
        Some((one, _)) => format!("{} ", one),
        _ => String::new(),
    }
}

const COMMANDS: [(&str, &str); 21] = [
    ("i", "Insert text to standard output."),
    ("a", "Append text to standard output."),
    ("d", "Delete the pattern space and start the next cycle."),
    ("D", "Delete the pattern space until the first newline and start the next cycle."),
    ("g", "Replace the contents of the pattern space by the contents of the hold space."),
    ("G", "Append to the pattern space a newline followed by the contents of the hold space."),
    ("h", "Replace the contents of the hold space with the contents of the pattern space."),
    ("H", "Append to the hold space a newline followed by the contents of the pattern space."),
    ("l", "Write the pattern space to standard output in a visually unambiguous form."),
    ("n", "Do default output, and read next line."),
    ("N", "Append the next input line to pattern space, with a newline added between."),
    ("p", "Write the pattern space to standard output."),
    ("P", "Write the pattern space, up to the first newline, to standard output."),
    ("x", "Exchange the contents of the pattern and hold spaces."),
    ("q", "Quit."),
    ("=", "Print the current line number."),
    (":", "Define a label."),
    ("b", "Jump to a label or the end of the script."),
    ("t", "Jump to a label or the end of the script if any substitution have been made since the most recent reading of an input line or execution of a t."),
    ("r", "Copy the content of given file to standard output."),
    ("w", "Append (write) the pattern space to given file."),
];

impl Cmd {
    fn format(&self, indent: usize) -> String {
        let mut s = String::new();
        let addr = fmt_address(&self.a);
        let neg = if self.neg { "!" } else { "" };
        let tabs = "\t".repeat(indent);

        match &self.c {
            Command::Block(block) => {
                s += &format!("{tabs}{addr}{neg}{{\n");
                let mut last = self.range.end.line;
                for c in block {
                    if c.range.start.line > last + 1 {
                        s += "\n";
                    }

                    s += &c.format(indent + 1);
                    s += "\n";

                    last = c.range.end.line;
                }
                s += &format!("{tabs}}}");
            }
            Command::Insert(text) => {
                s += &format!("{tabs}{addr}{neg}i\\\n");
                s += &text.join("\\\n");
            }
            Command::Append(text) => {
                s += &format!("{tabs}{addr}{neg}a\\\n");
                s += &text.join("\\\n");
            }
            Command::Change(text) => {
                s += &format!("{tabs}{addr}{neg}c\\\n");
                s += &text.join("\\\n");
            }
            Command::Silent(text) => {
                s += &format!("#n{text}");
            }
            Command::Comment(text) => {
                s += &format!("{tabs}#{text}");
            }
            Command::TwoAddrSimple(c) => {
                s += &format!("{tabs}{addr}{neg}{c}");
            }
            Command::Quit => {
                s += &format!("{tabs}{addr}{neg}q");
            }
            Command::CurLine => {
                s += &format!("{tabs}{addr}{neg}=");
            }
            Command::Label(label) => {
                s += &format!("{tabs}:{label}",);
            }
            Command::Branch(label) => {
                let sep = if label.is_empty() { "" } else { " " };
                s += &format!("{tabs}{addr}{neg}b{sep}{label}",);
            }
            Command::Test(label) => {
                let sep = if label.is_empty() { "" } else { " " };
                s += &format!("{tabs}{addr}{neg}t{sep}{label}",);
            }
            Command::Read(file_name) => {
                s += &format!("{tabs}{addr}{neg}r {file_name}");
            }
            Command::Write(file_name) => {
                s += &format!("{tabs}{addr}{neg}w {file_name}");
            }
            Command::Sub {
                re,
                delim,
                subs,
                flags,
                file,
            } => {
                let mut flags = flags.clone();
                flags.sort_unstable();
                let file_name = file
                    .as_ref()
                    .map(|s| " ".to_string() + s)
                    .unwrap_or_default();
                let flags = flags.join("");
                s += &format!(
                    "{tabs}{addr}{neg}s{delim}{re}{delim}{subs}{delim}{flags}{file_name}",
                );
            }
            Command::Replace { delim, from, to } => {
                s += &format!("{tabs}{addr}{neg}y{delim}{from}{delim}{to}{delim}",);
            }
        }
        s
    }

    fn hover(&self) -> Option<String> {
        match &self.c {
            Command::Insert(_) => Some("Insert text to standard output.".to_string()),
            Command::Append(_) => Some("Append text to standard output.".to_string()),
            Command::TwoAddrSimple(c) => match c {
                'd' => Some("Delete the pattern space and start the next cycle.".to_string()),
                'D' => Some("Delete the pattern space until the first newline and start the next cycle.".to_string()),
                'g' => Some("Replace the contents of the pattern space by the contents of the hold space.".to_string()),
                'G' => Some("Append to the pattern space a newline followed by the contents of the hold space.".to_string()),
                'h' => Some("Replace the contents of the hold space with the contents of the pattern space.".to_string()),
                'H' => Some("Append to the hold space a newline followed by the contents of the pattern space.".to_string()),
                'l' => Some("Write the pattern space to standard output in a visually unambiguous form.".to_string()),
                'n' => Some("Do default output, and read next line.".to_string()),
                'N' => Some("Append the next input line to pattern space, with a newline added between.".to_string()),
                'p' => Some("Write the pattern space to standard output.".to_string()),
                'P' => Some("Write the pattern space, up to the first newline, to standard output.".to_string()),
                'x' => Some("Exchange the contents of the pattern and hold spaces.".to_string()),
                _ => None,
            },
            Command::Quit => Some("Quit.".to_string()),
            Command::CurLine => Some("Print the current line number.".to_string()),
            Command::Label(label) => Some(format!("Defines the label `{label}`.")),
            Command::Branch(label) => {
                if label.is_empty() {
                    Some("Jump to the end of the script.".to_string())
                } else {
                    Some(format!("Jump to the label `{label}`."))
                }
            }
            Command::Test(label) => {
                if label.is_empty() {
                    Some("Jump to the end of the script if any substitution have been made since the most recent reading of an input line or execution of a t.".to_string())
                } else {
                    Some(format!("Jump to the label `{label}` if any substitution have been made since the most recent reading of an input line or execution of a t."))
                }
            }
            Command::Read(f) => Some(format!("Copy the content of file `{f}` to standard output.")),
            Command::Write(f) => Some(format!("Append (write) the pattern space to file `{f}`.")),
            Command::Silent(_) => Some(concat!("Suppress default output.\n", "Equivalent to specifying `-n` on the command line.").to_string()),
            Command::Sub { re, delim: _, subs, flags, file: _ } => {
                let how_many = if flags.iter().any(|f| f == "g") {
                    "all non-overlapping instances of".to_string()
                } else {
                    flags.iter().find(|f| f.chars().all(|c| c.is_ascii_digit())).map(|num| {
                        let suffix = match num.as_str() {
                            "1" => "st",
                            "2" => "nd",
                            "3" => "rd",
                            _ => "th",
                        };
                        format!("the {num}{suffix} occurence of")
                    }).unwrap_or_else(|| "first instance of".to_string())
                };
                let subs = if subs.is_empty() {
                    "the empty string".to_string()
                } else {
                    format!("`{subs}`")
                };
                let re = if re.is_empty() {
                    "the empty regex".to_string()
                } else {
                    format!("regular expression `{re}`")
                };
                Some(format!("Substitute {subs} for {how_many} {re} in the pattern space."))
            }
            Command::Replace { delim: _, from, to } =>
                Some(format!("Replace all characters from `{from}` with corresponding character in `{to}`.")),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct Program {
    commands: Vec<Cmd>,
}

fn find_at(cmd: &[Cmd], pos: &Position) -> Option<Cmd> {
    for c in cmd {
        if &c.range.start > pos {
            break;
        }
        if &c.range.start <= pos && &c.range.end > pos {
            return if let Command::Block(cs) = &c.c {
                find_at(cs, pos).or_else(|| Some(c.clone()))
            } else {
                Some(c.clone())
            };
        }
    }
    None
}

#[derive(Debug, Clone)]
struct Parser<'a> {
    src: &'a str,
    line: u32,
    col: u32,
    commands: Vec<Cmd>,
    diagnostics: Vec<Diagnostic>,
    label_defs: HashSet<String>,
    label_use: Vec<(String, Range)>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            line: 0,
            col: 0,
            commands: vec![],
            diagnostics: vec![],
            label_defs: HashSet::new(),
            label_use: vec![],
        }
    }

    fn diagnostic(&mut self, severity: DiagnosticSeverity, range: Range, message: String) {
        self.diagnostics.push(Diagnostic {
            range,
            message,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some("seducee".to_string()),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    fn warning(&mut self, range: Range, message: String) {
        self.diagnostic(DiagnosticSeverity::WARNING, range, message);
    }

    fn error(&mut self, range: Range, message: String) {
        self.diagnostic(DiagnosticSeverity::ERROR, range, message);
    }

    fn check_label_def(&mut self, label: &str, end: Position) {
        if label.is_empty() {
            let mut start = end;
            start.character -= 1;
            self.error(Range { start, end }, "Empty label.".to_string());
            return;
        }

        if !self.label_defs.insert(label.to_string()) {
            let mut start = end;
            start.character -= label.len() as u32;
            self.error(Range { start, end }, format!("Duplicate label `{label}`."));
        }

        self.check_branch_label(label, end);
        if label.starts_with(|c: char| c.is_whitespace()) {
            let x = label.trim_start();
            let mut start = end;
            start.character -= label.len() as u32;
            let mut end = end;
            end.character -= x.len() as u32;
            self.warning(
                Range { start, end },
                "Label starts with whitespace.".to_string(),
            );
        }
    }

    fn check_branch_label(&mut self, label: &str, end: Position) {
        if label.len() > 8 {
            let mut start = end;
            start.character -= (label.len() - 8) as u32;
            self.warning(
                Range { start, end },
                "Label longer than 8 bytes.".to_string(),
            );
        }
        if label.ends_with(|c: char| c.is_whitespace()) {
            let x = label.trim_end();
            let mut start = end;
            start.character -= (label.len() - x.len()) as u32;
            self.warning(Range { start, end }, "Whitespace after label.".to_string());
        }
        if let Some(i) = label.find(';') {
            let mut start = end;
            start.character -= (label.len() - i) as u32;
            let mut end = start;
            end.character += 1;
            self.warning(Range { start, end }, "Label contains a `;`.".to_string());
        }
    }

    fn opt_chars(&mut self, chars: &[char]) {
        let s = self.src.trim_start_matches(chars);
        self.col += (self.src.len() - s.len()) as u32;
        self.src = s;
    }

    fn opt_space(&mut self) {
        self.opt_chars(&[' ', '\t']);
    }

    fn opt_space_semi(&mut self) {
        self.opt_chars(&[' ', '\t', ';']);
    }

    fn expect(&mut self, c: char) -> bool {
        if self.src.starts_with(c) {
            self.src = &self.src[1..];
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            true
        } else {
            false
        }
    }

    fn block(&mut self) -> (bool, Vec<Cmd>) {
        let mut valid = true;
        let mut res = vec![];
        self.src = &self.src[1..];
        self.col += 1;

        while let Some(c) = self.command() {
            if valid && !c.valid {
                valid = false;
            }
            res.push(c);
        }

        if !self.src.starts_with('}') {
            valid = false;
        } else {
            self.src = &self.src[1..];
            self.col += 1;
            self.opt_space();
        }
        (valid, res)
    }

    fn re_addr(&mut self, delim: char) -> Addr {
        let mut esc = false;
        for (i, c) in self.src.char_indices().skip(1) {
            if c == '\n' {
                let re = self.src[1..i].to_string();
                self.src = &self.src[i + 1..];
                self.line += 1;
                self.col = 0;
                return Addr::Ctx {
                    re,
                    delim,
                    valid: false,
                };
            }
            if !esc && c == delim {
                let re = self.src[1..i].to_string();
                self.src = &self.src[i + 1..];
                self.col += i as u32 + 1;
                return Addr::Ctx {
                    re,
                    delim,
                    valid: true,
                };
            }

            esc = if esc { false } else { c == '\\' };
        }
        let re = self.src.to_string();
        self.src = "";
        Addr::Ctx {
            re,
            delim,
            valid: false,
        }
    }

    fn flag(&mut self) -> Option<String> {
        self.opt_space();
        match self.src.chars().next() {
            Some('0'..='9') => Some(self.num().to_string()),
            Some(c) if "gpw".contains(c) => {
                self.expect(c);
                Some(c.to_string())
            }
            _ => None,
        }
    }

    fn num(&mut self) -> usize {
        let mut n = 0;
        while let Some(c) = self.src.chars().next() {
            if !c.is_ascii_digit() {
                break;
            }
            n = 10 * n + (c as u8 - b'0') as usize;
            self.expect(c);
        }
        n
    }

    fn num_addr(&mut self) -> Addr {
        Addr::Num(self.num())
    }

    fn addr(&mut self) -> Option<Addr> {
        match self.src.chars().next() {
            Some('$') => {
                self.expect('$');
                Some(Addr::Last)
            }
            Some('\\') => {
                self.expect('\\');
                self.src.chars().next().map(|c| self.re_addr(c))
            }
            Some('/') => Some(self.re_addr('/')),
            Some(c) if c.is_ascii_digit() => Some(self.num_addr()),
            _ => None,
        }
    }

    fn two_addr(&mut self) -> Address {
        if let Some(one) = self.addr() {
            self.opt_space();
            if self.src.starts_with(',') {
                self.src = &self.src[1..];
                self.col += 1;
                self.opt_space();
                return self.addr().map(|two| (one, Some(two)));
            }
            return Some((one, None));
        }
        None
    }

    fn text(&mut self) -> Vec<String> {
        let mut res = vec![];
        loop {
            let line = self.rest_of_line();
            if let Some(content) = line.strip_suffix('\\') {
                res.push(content.to_string());
            } else {
                res.push(line.to_string());
                break;
            }
        }
        res
    }

    fn rest_of_line(&mut self) -> &str {
        let (s, rest) = self.src.split_once('\n').unwrap_or((self.src, ""));
        self.src = rest;
        self.line += 1;
        self.col = 0;
        s
    }

    fn command(&mut self) -> Option<Cmd> {
        loop {
            if self.src.is_empty() {
                return None;
            }

            self.opt_space_semi();
            if self.src.starts_with('\n') {
                self.src = &self.src[1..];
                self.line += 1;
                self.col = 0;
            } else {
                break;
            }
        }

        let start = Position {
            line: self.line,
            character: self.col,
        };

        let addr = self.two_addr();
        self.opt_space();

        let neg = self.expect('!');

        match self.src.chars().next().unwrap() {
            'i' => {
                self.expect('i');
                let s = self.rest_of_line();
                if s != "\\" {
                    // TODO: ...
                    return None;
                }
                let text = self.text();
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 1);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Insert(text),
                    range: Range { start, end },
                    valid,
                })
            }
            'a' => {
                self.expect('a');
                let s = self.rest_of_line();
                if s != "\\" {
                    // TODO: ...
                    return None;
                }
                let text = self.text();
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 1);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Append(text),
                    range: Range { start, end },
                    valid,
                })
            }
            'c' => {
                self.expect('c');
                let s = self.rest_of_line();
                if s != "\\" {
                    // TODO: ...
                    return None;
                }
                let text = self.text();
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 2);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Change(text),
                    range: Range { start, end },
                    valid,
                })
            }

            '{' => {
                let (valid, cs) = self.block();
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid && valid_address(&addr, 2);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Block(cs),
                    range: Range { start, end },
                    valid,
                })
            }

            '#' => {
                let mut end = Position {
                    line: self.line,
                    character: self.col,
                };
                let s = self.rest_of_line();
                end.character += s.len() as u32;
                let valid = !neg && addr.is_none();
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Comment(s[1..].to_string()),
                    range: Range { start, end },
                    valid,
                })
            }

            'q' => {
                self.expect('q');
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 1);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Quit,
                    range: Range { start, end },
                    valid,
                })
            }

            '=' => {
                self.expect('=');
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 1);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::CurLine,
                    range: Range { start, end },
                    valid,
                })
            }

            ':' => {
                self.expect(':');
                let mut valid = true;
                if !valid_address(&addr, 0) {
                    valid = false;
                    let mut end = start;
                    end.character = self.col - 1;
                    let mut start = start;
                    start.character = 0;
                    self.error(
                        Range { start, end },
                        "Label definition takes no address.".to_string(),
                    );
                }
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                valid = valid && !label.is_empty();
                self.check_label_def(&label, end);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Label(label),
                    range: Range { start, end },
                    valid,
                })
            }

            'b' => {
                self.expect('b');
                self.expect(' ');
                let valid = valid_address(&addr, 2);
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                self.check_branch_label(&label, end);
                self.label_use.push((label.clone(), Range { start, end }));
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Branch(label),
                    range: Range { start, end },
                    valid,
                })
            }

            't' => {
                self.expect('t');
                self.expect(' ');
                let valid = valid_address(&addr, 2);
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                self.check_branch_label(&label, end);
                self.label_use.push((label.clone(), Range { start, end }));
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Test(label),
                    range: Range { start, end },
                    valid,
                })
            }

            'r' => {
                self.expect('r');
                let col = self.col;
                self.opt_space();
                if self.col == col {
                    // TODO: diagnostic about missing space
                }
                let mut end = start;
                end.character = self.col;
                let file = self.rest_of_line().to_string();
                end.character += file.len() as u32;
                let valid = !file.is_empty() && valid_address(&addr, 1);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Read(file),
                    range: Range { start, end },
                    valid,
                })
            }

            'w' => {
                self.expect('w');
                let col = self.col;
                self.opt_space();
                if self.col == col {
                    // TODO: diagnostic about missing space
                }
                let mut end = start;
                end.character = self.col;
                let file = self.rest_of_line().to_string();
                end.character += file.len() as u32;
                let valid = !file.is_empty() && valid_address(&addr, 2);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Write(file),
                    range: Range { start, end },
                    valid,
                })
            }

            's' => {
                self.expect('s');
                let delim = self.src.chars().next().unwrap_or('\n');
                if delim == '\n' || delim == '\\' {
                    // TODO: invalid
                    return None;
                }

                let re = if let Addr::Ctx {
                    re,
                    delim: _,
                    valid: true,
                } = self.re_addr(delim)
                {
                    re
                } else {
                    return None;
                };

                let mut valid = false;
                let mut subs = String::new();

                let mut esc = false;
                for (i, c) in self.src.char_indices() {
                    if c == '\n' {
                        self.line += 1;
                        self.col = 0;
                    } else {
                        self.col += 1;
                    }

                    if !esc {
                        if c == '\n' {
                            subs = self.src[..i].to_string();
                            self.src = &self.src[i + 1..];
                            break;
                        } else if c == delim {
                            subs = self.src[..i].to_string();
                            self.src = &self.src[i + 1..];
                            valid = true;
                            break;
                        }
                    }

                    esc = if esc { false } else { c == '\\' };
                }

                let (valid, flags, file) = if valid {
                    self.sub_flags()
                } else {
                    (false, vec![], None)
                };

                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::Sub {
                        re,
                        delim,
                        subs,
                        flags,
                        file,
                    },
                    range: Range { start, end },
                    valid,
                })
            }

            'y' => {
                self.expect('y');
                let delim = self.src.chars().next().unwrap_or('\n');
                if delim == '\n' || delim == '\\' {
                    // TODO: invalid
                    return None;
                }

                self.expect(delim);
                let from_start = Position {
                    line: self.line,
                    character: self.col,
                };
                if let Some(from) = self.until(delim) {
                    let to_start = Position {
                        line: self.line,
                        character: self.col,
                    };
                    if let Some(to) = self.until(delim) {
                        let end = Position {
                            line: self.line,
                            character: self.col,
                        };
                        let mut valid = true;
                        let mut esc = false;
                        let mut from_chars = vec![];
                        for (i, mut c) in from.chars().enumerate() {
                            if esc {
                                if c != delim && c != 'n' && c != '\\' {
                                    valid = false;
                                    self.warning(
                                        Range {
                                            start: Position {
                                                line: from_start.line,
                                                character: from_start.character + i as u32,
                                            },
                                            end: Position {
                                                line: from_start.line,
                                                character: from_start.character + i as u32 + 1,
                                            },
                                        },
                                        format!("unknown escape `{c}`, the backslash is treated literally"),
                                    );
                                    from_chars.push('\\');
                                }
                                esc = false;
                                if c == 'n' {
                                    c = '\n'
                                }
                            } else if c == '\\' {
                                esc = true;
                                continue;
                            }
                            if from_chars.contains(&c) {
                                valid = false;
                                self.warning(
                                    Range {
                                        start: Position {
                                            line: from_start.line,
                                            character: from_start.character + i as u32,
                                        },
                                        end: Position {
                                            line: from_start.line,
                                            character: from_start.character + i as u32 + 1,
                                        },
                                    },
                                    if c == '\n' {
                                        "duplicate character `\\n`".to_string()
                                    } else {
                                        format!("duplicate character `{c}`")
                                    },
                                );
                            }
                            from_chars.push(c);
                        }

                        let mut n = 0;
                        for (i, c) in to.chars().enumerate() {
                            if esc {
                                if c != delim && c != 'n' && c != '\\' {
                                    valid = false;
                                    self.warning(
                                        Range {
                                            start: Position {
                                                line: to_start.line,
                                                character: to_start.character + i as u32,
                                            },
                                            end: Position {
                                                line: to_start.line,
                                                character: to_start.character + i as u32 + 1,
                                            },
                                        },
                                        format!("unknown escape `{c}`, the backslash is treated literally"),
                                    );
                                    n += 1;
                                }
                                esc = false;
                                n += 1;
                            } else if c == '\\' {
                                esc = true;
                            } else {
                                n += 1;
                            }
                        }
                        if n != from_chars.len() {
                            valid = false;
                            self.error(
                                Range {
                                    start: from_start,
                                    end,
                                },
                                format!("Transform strings are not the same length: the first has {} characters and the second has {}.", from_chars.len(), n),
                            );
                        }
                        return Some(Cmd {
                            a: addr,
                            neg,
                            c: Command::Replace { delim, from, to },
                            range: Range { start, end },
                            valid,
                        });
                    }
                }
                None
            }

            c if "dDgGhHlnNpPx".contains(c) => {
                self.expect(c);
                let end = Position {
                    line: self.line,
                    character: self.col,
                };
                let valid = valid_address(&addr, 2);
                Some(Cmd {
                    a: addr,
                    neg,
                    c: Command::TwoAddrSimple(c),
                    range: Range { start, end },
                    valid,
                })
            }

            _ => None,
        }
    }

    fn program(&mut self) {
        if self.src.starts_with("#n") {
            let (s, rest) = self.src.split_once('\n').unwrap_or((self.src, ""));
            self.src = rest;
            self.commands.push(Cmd {
                a: None,
                neg: false,
                c: Command::Silent(s[2..].to_string()),
                range: Range {
                    start: Position {
                        line: self.line,
                        character: 0,
                    },
                    end: Position {
                        line: self.line,
                        character: s.len() as u32,
                    },
                },
                valid: true,
            });
            self.line += 1;
        }

        while let Some(c) = self.command() {
            self.commands.push(c);
        }

        for (label, range) in self.label_use.clone() {
            if !label.is_empty() && !self.label_defs.contains(&label) {
                self.error(range, format!("Undefined label `{label}`"));
            }
        }

        // NOTE: any left overs from parsing is stored as a single invalid comment
        if !self.src.is_empty() {
            self.commands.push(Cmd {
                a: None,
                neg: false,
                c: Command::Comment(self.src.to_string()),
                range: Range {
                    start: Position {
                        line: self.line,
                        character: self.col,
                    },
                    end: Position {
                        line: self.line + self.src.lines().count() as u32 + 1,
                        character: 0,
                    },
                },
                valid: false,
            });
        }
    }

    fn until(&mut self, delim: char) -> Option<String> {
        let mut prev = 'x';
        for (i, c) in self.src.char_indices() {
            self.col += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 0;
                return None;
            }
            if prev != '\\' && c == delim {
                let s = self.src[..i].to_string();
                self.src = &self.src[i + 1..];
                return Some(s);
            }

            prev = c;
        }

        None
    }

    fn sub_flags(&mut self) -> (bool, Vec<String>, Option<String>) {
        // TODO: duplicate flags not checked
        let mut valid = true;
        let mut file = None;
        let mut flags = vec![];

        let start = Position {
            line: self.line,
            character: self.col,
        };

        while let Some(flag) = self.flag() {
            let write = flag.as_str() == "w";
            flags.push(flag);
            if write {
                self.expect(' ');
                let name = self.rest_of_line().to_string();
                if name.is_empty() {
                    valid = false;
                }
                file = Some(name);
                break;
            }
        }

        let g_and_numbers = flags
            .iter()
            .filter(|&f| *f == "g" || f.chars().all(|c| c.is_ascii_digit()))
            .count();
        if g_and_numbers > 1 {
            self.error(
                Range {
                    start,
                    end: Position {
                        line: self.line,
                        character: self.col,
                    },
                },
                "More than one `g` or number flag.".to_string(),
            );
        }

        (valid, flags, file)
    }
}

impl Program {
    fn range(&self) -> Range {
        let zero = Position {
            line: 0,
            character: 0,
        };
        let start = self.commands.first().map_or(zero, |c| c.range.start);
        let end = self.commands.last().map_or(zero, |c| c.range.end);
        Range { start, end }
    }

    fn format(&self) -> String {
        let mut s = String::new();
        let mut last = 0;
        for c in &self.commands {
            if c.range.start.line > last + 1 {
                s += "\n";
            }

            s += &c.format(0);
            s += "\n";

            last = c.range.end.line;
        }
        if let Some(c) = self.commands.first() {
            if matches!(c.c, Command::Comment(_)) && s.starts_with("#n") {
                s.insert(0, ';');
            }
        }
        s
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    programs: DashMap<String, Program>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            programs: DashMap::default(),
        }
    }

    async fn changed_document(&self, uri: Url, text: &str) {
        let mut p = Parser::new(text);
        p.program();
        let prog = Program {
            commands: p.commands,
        };
        self.programs.insert(uri.to_string(), prog);
        self.client
            .publish_diagnostics(uri, p.diagnostics, None)
            .await;
    }

    fn labels(&self, cmd: &[Cmd], label: &str) -> Vec<(Range, &str)> {
        cmd.iter()
            .flat_map(|c| match &c.c {
                Command::Block(cs) => self.labels(cs, label),
                Command::Label(l) if l == label => vec![(c.range, ":")],
                Command::Branch(l) if l == label => vec![(c.range, "b ")],
                Command::Test(l) if l == label => vec![(c.range, "t ")],
                _ => vec![],
            })
            .collect()
    }

    fn definition(&self, cmd: &[Cmd], label: &str) -> Option<Range> {
        let mut it = cmd.iter().filter_map(|c| match &c.c {
            Command::Label(l) if l == label => Some(c.range),
            Command::Block(cs) => self.definition(cs, label),
            _ => None,
        });
        let res = it.next();
        if it.next().is_some() {
            None
        } else {
            res
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: None,
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = &params.text_document.text;
        self.changed_document(uri, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = &params.content_changes[0].text;
        self.changed_document(uri, text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.as_str();
        self.programs.remove(uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let params = params.text_document_position;
        let pos = params.position;
        let prog = self
            .programs
            .get(params.text_document.uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        let is_comment = pos.character > 0
            && matches!(
                find_at(
                    &prog.commands,
                    &Position {
                        line: pos.line,
                        character: pos.character - 1
                    }
                ),
                Some(Cmd {
                    c: Command::Comment(_),
                    ..
                })
            );

        if is_comment {
            if pos.line == 0 && pos.character == 1 {
                return Ok(Some(CompletionResponse::Array(vec![CompletionItem {
                    label: "#n".to_string(),
                    insert_text: Some("n".to_string()),
                    detail: Some(
                        concat!(
                            "Suppress default output.\n",
                            "Equivalent to specifying `-n` on the command line."
                        )
                        .to_string(),
                    ),
                    ..CompletionItem::default()
                }])));
            }
            return Ok(None);
        }

        let mut completions: Vec<_> = COMMANDS
            .iter()
            .map(|(label, doc)| CompletionItem {
                label: label.to_string(),
                detail: Some(doc.to_string()),
                ..CompletionItem::default()
            })
            .collect();
        if pos.line == 0 && pos.character == 0 {
            completions.push(CompletionItem {
                label: "#n".to_string(),
                detail: Some(
                    concat!(
                        "Suppress default output.\n",
                        "Equivalent to specifying `-n` on the command line."
                    )
                    .to_string(),
                ),
                ..CompletionItem::default()
            })
        }

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let params = params.text_document_position_params;
        let prog = self
            .programs
            .get(params.text_document.uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;
        let pos = params.position;
        if let Some(c) = find_at(&prog.commands, &pos) {
            return Ok(c.hover().map(|text| Hover {
                contents: HoverContents::Scalar(MarkedString::String(text)),
                range: Some(c.range),
            }));
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let params = params.text_document_position_params;
        let uri = params.text_document.uri;
        let prog = self
            .programs
            .get(uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        let pos = params.position;
        if let Some(c) = find_at(&prog.commands, &pos) {
            let res = match &c.c {
                Command::Branch(label) | Command::Test(label) => self
                    .definition(&prog.commands, label)
                    .map(|range| GotoDefinitionResponse::Scalar(Location { uri, range })),
                _ => None,
            };
            return Ok(res);
        }
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let params = params.text_document_position;
        let uri = params.text_document.uri;
        let prog = self
            .programs
            .get(uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        let pos = params.position;
        if let Some(c) = find_at(&prog.commands, &pos) {
            let res = match &c.c {
                Command::Label(label) | Command::Branch(label) | Command::Test(label) => Some(
                    self.labels(&prog.commands, label)
                        .into_iter()
                        .map(|(range, _)| Location {
                            uri: uri.clone(),
                            range,
                        })
                        .collect(),
                ),
                _ => None,
            };
            return Ok(res);
        }
        Ok(None)
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let params = params.text_document_position_params;
        let prog = self
            .programs
            .get(params.text_document.uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        let pos = params.position;
        if let Some(c) = find_at(&prog.commands, &pos) {
            let range = c.range;
            let hl = match &c.c {
                Command::Label(label) | Command::Branch(label) | Command::Test(label) => self
                    .labels(&prog.commands, label)
                    .into_iter()
                    .map(|(range, _)| DocumentHighlight { range, kind: None })
                    .collect(),
                _ => vec![DocumentHighlight { range, kind: None }],
            };
            return Ok(Some(hl));
        }
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        if params.options.insert_spaces {
            return Err(Error::invalid_params("must prefer tabs"));
        }

        let prog = self
            .programs
            .get(params.text_document.uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        if !prog.commands.iter().all(|c| c.valid) {
            self.client
                .show_message(MessageType::WARNING, "won't format invalid document")
                .await;
            return Ok(None);
        }

        let mut range = prog.range();
        range.start.line = 0;
        range.start.character = 0;
        // NOTE: this will not trim unlimited number of trailing empty lines
        range.end.line += 1000;
        range.end.character = 0;
        let new_text = prog.format();
        Ok(Some(vec![TextEdit { range, new_text }]))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let new_name = params.new_name;
        let params = params.text_document_position;
        let uri = params.text_document.uri;
        let prog = self
            .programs
            .get(uri.as_str())
            .ok_or_else(|| Error::invalid_params("document not found"))?;

        if self.definition(&prog.commands, &new_name).is_some() {
            return Err(Error::invalid_params(format!(
                "The label `{new_name}` is already defined."
            )));
        }

        let pos = params.position;
        if let Some(c) = find_at(&prog.commands, &pos) {
            let res = match &c.c {
                Command::Label(label) | Command::Branch(label) | Command::Test(label) => self
                    .labels(&prog.commands, label)
                    .into_iter()
                    .map(|(range, tag)| TextEdit {
                        range,
                        new_text: format!("{tag}{new_name}"),
                    })
                    .collect(),
                _ => return Err(Error::invalid_params("Can only rename labels.")),
            };
            return Ok(Some(WorkspaceEdit {
                changes: Some(HashMap::from([(uri, res)])),
                document_changes: None,
                change_annotations: None,
            }));
        }
        Err(Error::invalid_params("Can only rename labels."))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
