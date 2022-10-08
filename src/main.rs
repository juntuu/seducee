use std::collections::HashMap;
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
    match &addr {
        &Some((one, Some(two))) => n >= 2 && valid_addr(one) && valid_addr(two),
        &Some((one, None)) => n >= 1 && valid_addr(one),
        &None => true,
    }
}

fn fmt_address(addr: &Address) -> String {
    match addr {
        Some((one, Some(two))) => format!("{}, {} ", one, two),
        Some((one, _)) => format!("{} ", one),
        _ => String::new(),
    }
}

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
            Command::Block(_) => None,
            Command::Insert(_) => Some("Insert text to standard output.".to_string()),
            Command::Append(_) => Some("Append text to standard output.".to_string()),
            Command::Change(_) => None,
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
            Command::Label(label) => Some(format!("Defines a label `{label}`.")),
            Command::Branch(label) => {
                if label.is_empty() {
                    Some(format!("Jump to the end of the script."))
                } else {
                    Some(format!("Jump to the label `{label}`."))
                }
            }
            Command::Test(label) => {
                if label.is_empty() {
                    Some(format!("Jump to the end of the script if any substitution have been made since the most recent reading of an input line or execution of a t."))
                } else {
                    Some(format!("Jump to the label `{label}` if any substitution have been made since the most recent reading of an input line or execution of a t."))
                }
            }
            Command::Read(f) => Some(format!(
                "Copy the content of file `{f}` to standard output."
            )),
            Command::Write(f) => Some(format!("Append (write) the pattern space to file `{f}`.")),
            Command::Sub {
                re,
                delim,
                subs,
                flags,
                file,
            } => None,
            Command::Replace { delim, from, to } => None,
            Command::Silent(_) => Some(
                concat!(
                    "Suppress default output.\n",
                    "Equivalent to specifying `-n` on the command line."
                )
                .to_string(),
            ),
            Command::Comment(_) => None,
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
struct Diagnostic {
    range: Range,
    message: String,
}

#[derive(Debug, Clone)]
struct Parser<'a> {
    src: &'a str,
    line: u32,
    col: u32,
    commands: Vec<Cmd>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            line: 0,
            col: 0,
            commands: vec![],
            diagnostics: vec![],
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
        return Addr::Ctx {
            re,
            delim,
            valid: false,
        };
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
            self.src = &self.src[1..];
        }
        n
    }

    fn num_addr(&mut self) -> Addr {
        Addr::Num(self.num())
    }

    fn addr(&mut self) -> Option<Addr> {
        match self.src.chars().next() {
            Some('$') => {
                self.src = &self.src[1..];
                Some(Addr::Last)
            }
            Some('\\') => {
                self.src = &self.src[1..];
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

        let neg = if self.src.starts_with('!') {
            self.src = &self.src[1..];
            true
        } else {
            false
        };

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
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                let valid = !label.is_empty() && valid_address(&addr, 0);
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
                let valid = self.expect(' ');
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                let valid = valid && !label.is_empty() && valid_address(&addr, 2);
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
                let valid = self.expect(' ');
                let mut end = start;
                end.character = self.col;
                let label = self.rest_of_line().to_string();
                end.character += label.len() as u32;
                let valid = valid && !label.is_empty() && valid_address(&addr, 2);
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
                if let Some(from) = self.until(delim) {
                    if let Some(to) = self.until(delim) {
                        let valid = self.check_repl(delim, &from, &to);
                        let end = Position {
                            line: self.line,
                            character: self.col,
                        };
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

    fn check_repl(&self, delim: char, from: &str, to: &str) -> bool {
        let mut esc = false;
        let mut from_chars = vec![];
        for c in from.chars() {
            if esc {
                if c != delim && c != 'n' && c != '\\' {
                    return false;
                }
                esc = false;
                let c = if c == 'n' { '\n' } else { c };
                if from_chars.contains(&c) {
                    return false;
                }
                from_chars.push(c);
            } else if c == '\\' {
                esc = true;
            } else if from_chars.contains(&c) {
                return false;
            } else {
                from_chars.push(c);
            }
        }

        let mut n = 0;
        for c in to.chars() {
            if esc {
                if c != delim && c != 'n' && c != '\\' {
                    return false;
                }
                esc = false;
                n += 1;
            } else if c == '\\' {
                esc = true;
            } else {
                n += 1;
            }
            if n > from_chars.len() {
                return false;
            }
        }

        true
    }

    fn until(&mut self, delim: char) -> Option<String> {
        let mut prev = 'x';
        for (i, c) in self.src.char_indices() {
            if prev != '\\' {
                if c == '\n' {
                    return None;
                }
                if c == delim {
                    let s = self.src[..i].to_string();
                    self.src = &self.src[i + 1..];
                    return Some(s);
                }
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

    async fn verbose_error(&self, message: &str) -> Error {
        self.client.show_message(MessageType::ERROR, message).await;
        Error::method_not_found()
    }

    async fn changed_document(&self, uri: &str, text: &str) {
        // TODO: emit diagnostics from parsing
        let mut p = Parser::new(text);
        p.program();
        let prog = Program {
            commands: p.commands,
        };
        self.client
            .show_message(MessageType::INFO, format!("{:?}", prog))
            .await;
        self.programs.insert(uri.to_string(), prog);
    }

    fn labels(&self, cmd: &[Cmd], label: &str) -> Vec<(Range, &str)> {
        cmd.iter()
            .filter_map(|c| match &c.c {
                Command::Label(l) if l == label => Some((c.range, ":")),
                Command::Branch(l) if l == label => Some((c.range, "b ")),
                Command::Test(l) if l == label => Some((c.range, "t ")),
                _ => None,
            })
            .collect()
    }

    fn definition(&self, cmd: &[Cmd], label: &str) -> Option<Range> {
        let mut it = cmd.iter().filter_map(|c| match &c.c {
            Command::Label(l) if l == label => Some(c.range),
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
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![":".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                semantic_tokens_provider: None,
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: None,
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                }),
                document_highlight_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
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
        let uri = params.text_document.uri.as_str();
        let text = &params.text_document.text;
        self.changed_document(uri, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.as_str();
        let text = &params.content_changes[0].text;
        self.changed_document(uri, text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.as_str();
        self.programs.remove(uri);
    }

    /// The [`textDocument/completion`] request is sent from the client to the server to compute
    /// completion items at a given cursor position.
    ///
    /// If computing full completion items is expensive, servers can additionally provide a handler
    /// for the completion item resolve request (`completionItem/resolve`). This request is sent
    /// when a completion item is selected in the user interface.
    ///
    /// [`textDocument/completion`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_completion
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let _ = params;
        Ok(Some(
            vec![
                CompletionItem::new_simple("hello".to_string(), "what should I say".to_string()),
                CompletionItem::new_simple("world".to_string(), "...".to_string()),
            ]
            .into(),
        ))
    }

    /// The [`textDocument/hover`] request asks the server for hover information at a given text
    /// document position.
    ///
    /// Such hover information typically includes type signature information and inline
    /// documentation for the symbol at the given text document position.
    ///
    /// [`textDocument/hover`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_hover
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

    /// The [`textDocument/definition`] request asks the server for the definition location of a
    /// symbol at a given text document position.
    ///
    /// [`textDocument/definition`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_definition
    ///
    /// # Compatibility
    ///
    /// The [`GotoDefinitionResponse::Link`](lsp_types::GotoDefinitionResponse::Link) return value
    /// was introduced in specification version 3.14.0 and requires client-side support in order to
    /// be used. It can be returned if the client set the following field to `true` in the
    /// [`initialize`](LanguageServer::initialize) method:
    ///
    /// ```text
    /// InitializeParams::capabilities::text_document::definition::link_support
    /// ```
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

    /// The [`textDocument/references`] request is sent from the client to the server to resolve
    /// project-wide references for the symbol denoted by the given text document position.
    ///
    /// [`textDocument/references`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_references
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

    /// The [`textDocument/formatting`] request is sent from the client to the server to format a
    /// whole document.
    ///
    /// [`textDocument/formatting`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_formatting
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
        // NOTE: this will not trim unlimited number of trailing empty lines
        range.end.line += 1000;
        range.end.character = 0;
        let new_text = prog.format();

        self.client
            .show_message(MessageType::WARNING, format!("{:?}\n{:?}", range, new_text))
            .await;

        Ok(Some(vec![TextEdit { range, new_text }]))
    }

    /// The [`textDocument/rename`] request is sent from the client to the server to ask the server
    /// to compute a workspace change so that the client can perform a workspace-wide rename of a
    /// symbol.
    ///
    /// [`textDocument/rename`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_rename
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
            let mut map = HashMap::new();
            map.insert(uri, res);
            return Ok(Some(WorkspaceEdit {
                changes: Some(map),
                document_changes: None,
                change_annotations: None,
            }));
        }
        Err(Error::invalid_params("Can only rename labels."))
    }

    /// The [`textDocument/semanticTokens/full`] request is sent from the client to the server to
    /// resolve the semantic tokens of a given file.
    ///
    /// Semantic tokens are used to add additional color information to a file that depends on
    /// language specific symbol information. A semantic token request usually produces a large
    /// result. The protocol therefore supports encoding tokens with numbers. In addition, optional
    /// support for deltas is available, i.e. [`semantic_tokens_full_delta`].
    ///
    /// [`textDocument/semanticTokens/full`]: https://microsoft.github.io/language-server-protocol/specification#textDocument_semanticTokens
    /// [`semantic_tokens_full_delta`]: LanguageServer::semantic_tokens_full_delta
    ///
    /// # Compatibility
    ///
    /// This request was introduced in specification version 3.16.0.
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let _ = params;
        Err(self.verbose_error("semantic_tokens_full").await)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
