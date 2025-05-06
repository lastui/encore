#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Characters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Question,
    Arrow,
    Hash,
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Star,
    StarStar,
    StarEqual,
    StarStarEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Bang,
    BangEqual,
    BangEqualEqual,
    Greater,
    GreaterEqual,
    GreaterGreater,
    GreaterGreaterEqual,
    GreaterGreaterGreater,
    GreaterGreaterGreaterEqual,
    Less,
    LessEqual,
    LessLess,
    LessLessEqual,
    Ampersand,
    AmpersandEqual,
    AmpersandAmpersand,
    AmpersandAmpersandEqual,
    Pipe,
    PipeEqual,
    PipePipe,
    PipePipeEqual,
    Caret,
    CaretEqual,
    Tilde,
    Ellipsis,
    QuestionQuestion,
    QuestionQuestionEqual,
    QuestionDot,
    // Literals
    Identifier(String),
    StringLiteral(String),
    TemplateLiteral(Vec<TemplatePart>),
    NumberLiteral(f64),
    BigIntLiteral(String),
    RegExpLiteral(String, String),
    True,
    False,
    Null,
    Undefined,
    // Keywords
    Var,
    Let,
    With,
    Const,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    This,
    Super,
    New,
    Delete,
    Typeof,
    Void,
    In,
    InstanceOf,
    Try,
    Catch,
    Finally,
    Throw,
    Switch,
    Case,
    Default,
    Await,
    Async,
    Do,
    Enum,
    Of,
    Target,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public,
    Arguments,
    Eval,
    Debugger,
    Class,
    Extends,
    Constructor,
    Static,
    Get,
    Set,
    Yield,
    Import,
    Export,
    From,
    As,
    // Sentinel
    EOF,
}

impl Token {

    pub fn keyword_text(&self) -> Option<&str> {
        match self {
            // Characters/operators don't have keyword text
            Token::LeftParen | Token::RightParen | Token::LeftBrace | 
            Token::RightBrace | Token::LeftBracket | Token::RightBracket | 
            Token::Comma | Token::Dot | Token::Semicolon | Token::Colon | 
            Token::Question | Token::Arrow | Token::Hash | Token::Plus | 
            Token::PlusPlus | Token::PlusEqual | Token::Minus | 
            Token::MinusMinus | Token::MinusEqual | Token::Star | 
            Token::StarStar | Token::StarEqual | Token::StarStarEqual | 
            Token::Slash | Token::SlashEqual | Token::Percent | 
            Token::PercentEqual | Token::Equal | Token::EqualEqual | 
            Token::EqualEqualEqual | Token::Bang | Token::BangEqual | 
            Token::BangEqualEqual | Token::Greater | Token::GreaterEqual | 
            Token::GreaterGreater | Token::GreaterGreaterEqual | 
            Token::GreaterGreaterGreater | Token::GreaterGreaterGreaterEqual | 
            Token::Less | Token::LessEqual | Token::LessLess | 
            Token::LessLessEqual | Token::Ampersand | Token::AmpersandEqual | 
            Token::AmpersandAmpersand | Token::AmpersandAmpersandEqual | 
            Token::Pipe | Token::PipeEqual | Token::PipePipe | 
            Token::PipePipeEqual | Token::Caret | Token::CaretEqual | 
            Token::Tilde | Token::Ellipsis | Token::QuestionQuestion | 
            Token::QuestionQuestionEqual | Token::QuestionDot => None,
            
            // Literals don't have keyword text
            Token::Identifier(_) | Token::StringLiteral(_) | 
            Token::TemplateLiteral(_) | Token::NumberLiteral(_) | 
            Token::BigIntLiteral(_) | Token::RegExpLiteral(_, _) => None,
            
            // Boolean literals and null
            Token::True => Some("true"),
            Token::False => Some("false"),
            Token::Null => Some("null"),
            Token::Undefined => Some("undefined"),
            
            // Keywords
            Token::Var => Some("var"),
            Token::Let => Some("let"),
            Token::With => Some("with"),
            Token::Const => Some("const"),
            Token::Function => Some("function"),
            Token::Return => Some("return"),
            Token::If => Some("if"),
            Token::Else => Some("else"),
            Token::While => Some("while"),
            Token::For => Some("for"),
            Token::Break => Some("break"),
            Token::Continue => Some("continue"),
            Token::This => Some("this"),
            Token::Super => Some("super"),
            Token::New => Some("new"),
            Token::Delete => Some("delete"),
            Token::Typeof => Some("typeof"),
            Token::Void => Some("void"),
            Token::In => Some("in"),
            Token::InstanceOf => Some("instanceof"),
            Token::Try => Some("try"),
            Token::Catch => Some("catch"),
            Token::Finally => Some("finally"),
            Token::Throw => Some("throw"),
            Token::Switch => Some("switch"),
            Token::Case => Some("case"),
            Token::Default => Some("default"),
            Token::Await => Some("await"),
            Token::Async => Some("async"),
            Token::Do => Some("do"),
            Token::Enum => Some("enum"),
            Token::Of => Some("of"),
            Token::Target => Some("target"),
            Token::Implements => Some("implements"),
            Token::Interface => Some("interface"),
            Token::Package => Some("package"),
            Token::Private => Some("private"),
            Token::Protected => Some("protected"),
            Token::Public => Some("public"),
            Token::Arguments => Some("arguments"),
            Token::Eval => Some("eval"),
            Token::Debugger => Some("debugger"),
            Token::Class => Some("class"),
            Token::Extends => Some("extends"),
            Token::Constructor => Some("constructor"),
            Token::Static => Some("static"),
            Token::Get => Some("get"),
            Token::Set => Some("set"),
            Token::Yield => Some("yield"),
            Token::Import => Some("import"),
            Token::Export => Some("export"),
            Token::From => Some("from"),
            Token::As => Some("as"),
            
            // Sentinel
            Token::EOF => None,
        }
    }
    
    pub fn to_string(&self) -> String {
        match self {
            Token::Identifier(name) => name.clone(),
            Token::StringLiteral(s) => format!("\"{}\"", s),
            Token::NumberLiteral(n) => n.to_string(),
            Token::BigIntLiteral(b) => format!("{}n", b),
            Token::RegExpLiteral(pattern, flags) => format!("/{}/{}", pattern, flags),
            Token::TemplateLiteral(_) => "`...`".to_string(),
            _ => match self.keyword_text() {
                Some(text) => text.to_string(),
                None => format!("{:?}", self),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    String(String),
    Expression(String),
}
