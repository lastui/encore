use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum FormatStyle {
    Compact,
    Pretty {
        indent_size: usize,
    },
}

pub struct Formatter {
    buffer: String,
    style: FormatStyle,
    current_indent: usize,
    line_start: bool,
}

impl Formatter {
    pub fn new(style: FormatStyle) -> Self {
        Self {
            buffer: String::new(),
            style,
            current_indent: 0,
            line_start: true,
        }
    }

    pub fn write_str(&mut self, s: &str) {
        if self.line_start {
            match &self.style {
                FormatStyle::Pretty { indent_size } => {
                    for _ in 0..self.current_indent * indent_size {
                        self.buffer.push(' ');
                    }
                },
                FormatStyle::Compact => {}
            }
            self.line_start = false;
        }
        self.buffer.push_str(s);
    }

    pub fn write_char(&mut self, c: char) {
        if self.line_start {
            match &self.style {
                FormatStyle::Pretty { indent_size } => {
                    for _ in 0..self.current_indent * indent_size {
                        self.buffer.push(' ');
                    }
                },
                FormatStyle::Compact => {}
            }
            self.line_start = false;
        }
        self.buffer.push(c);
    }

    pub fn newline(&mut self) {
        match self.style {
            FormatStyle::Pretty { .. } => {
                self.buffer.push('\n');
                self.line_start = true;
            },
            FormatStyle::Compact => {
            }
        }
    }

    pub fn space(&mut self) {
        match self.style {
            FormatStyle::Pretty { .. } => {
                self.buffer.push(' ');
            },
            FormatStyle::Compact => {}
        }
    }

    pub fn undefined(&mut self) {
        match self.style {
            FormatStyle::Pretty { .. } => {
                self.write_str("undefined");
            },
            FormatStyle::Compact => {
                self.write_str("void 0");
            }
        }
    }

    pub fn indent(&mut self) {
        self.current_indent += 1;
    }

    pub fn dedent(&mut self) {
        if self.current_indent > 0 {
            self.current_indent -= 1;
        }
    }

    pub fn as_str(&self) -> &str {
        &self.buffer
    }
}

impl Write for Formatter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.write_str(s);
        Ok(())
    }
}
