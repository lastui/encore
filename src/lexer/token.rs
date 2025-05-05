#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
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

impl TokenType {

    pub fn keyword_text(&self) -> Option<&str> {
        match self {
            // Characters/operators don't have keyword text
            TokenType::LeftParen | TokenType::RightParen | TokenType::LeftBrace | 
            TokenType::RightBrace | TokenType::LeftBracket | TokenType::RightBracket | 
            TokenType::Comma | TokenType::Dot | TokenType::Semicolon | TokenType::Colon | 
            TokenType::Question | TokenType::Arrow | TokenType::Hash | TokenType::Plus | 
            TokenType::PlusPlus | TokenType::PlusEqual | TokenType::Minus | 
            TokenType::MinusMinus | TokenType::MinusEqual | TokenType::Star | 
            TokenType::StarStar | TokenType::StarEqual | TokenType::StarStarEqual | 
            TokenType::Slash | TokenType::SlashEqual | TokenType::Percent | 
            TokenType::PercentEqual | TokenType::Equal | TokenType::EqualEqual | 
            TokenType::EqualEqualEqual | TokenType::Bang | TokenType::BangEqual | 
            TokenType::BangEqualEqual | TokenType::Greater | TokenType::GreaterEqual | 
            TokenType::GreaterGreater | TokenType::GreaterGreaterEqual | 
            TokenType::GreaterGreaterGreater | TokenType::GreaterGreaterGreaterEqual | 
            TokenType::Less | TokenType::LessEqual | TokenType::LessLess | 
            TokenType::LessLessEqual | TokenType::Ampersand | TokenType::AmpersandEqual | 
            TokenType::AmpersandAmpersand | TokenType::AmpersandAmpersandEqual | 
            TokenType::Pipe | TokenType::PipeEqual | TokenType::PipePipe | 
            TokenType::PipePipeEqual | TokenType::Caret | TokenType::CaretEqual | 
            TokenType::Tilde | TokenType::Ellipsis | TokenType::QuestionQuestion | 
            TokenType::QuestionQuestionEqual | TokenType::QuestionDot => None,
            
            // Literals don't have keyword text
            TokenType::Identifier(_) | TokenType::StringLiteral(_) | 
            TokenType::TemplateLiteral(_) | TokenType::NumberLiteral(_) | 
            TokenType::BigIntLiteral(_) | TokenType::RegExpLiteral(_, _) => None,
            
            // Boolean literals and null
            TokenType::True => Some("true"),
            TokenType::False => Some("false"),
            TokenType::Null => Some("null"),
            TokenType::Undefined => Some("undefined"),
            
            // Keywords
            TokenType::Var => Some("var"),
            TokenType::Let => Some("let"),
            TokenType::With => Some("with"),
            TokenType::Const => Some("const"),
            TokenType::Function => Some("function"),
            TokenType::Return => Some("return"),
            TokenType::If => Some("if"),
            TokenType::Else => Some("else"),
            TokenType::While => Some("while"),
            TokenType::For => Some("for"),
            TokenType::Break => Some("break"),
            TokenType::Continue => Some("continue"),
            TokenType::This => Some("this"),
            TokenType::Super => Some("super"),
            TokenType::New => Some("new"),
            TokenType::Delete => Some("delete"),
            TokenType::Typeof => Some("typeof"),
            TokenType::Void => Some("void"),
            TokenType::In => Some("in"),
            TokenType::InstanceOf => Some("instanceof"),
            TokenType::Try => Some("try"),
            TokenType::Catch => Some("catch"),
            TokenType::Finally => Some("finally"),
            TokenType::Throw => Some("throw"),
            TokenType::Switch => Some("switch"),
            TokenType::Case => Some("case"),
            TokenType::Default => Some("default"),
            TokenType::Await => Some("await"),
            TokenType::Async => Some("async"),
            TokenType::Do => Some("do"),
            TokenType::Enum => Some("enum"),
            TokenType::Of => Some("of"),
            TokenType::Target => Some("target"),
            TokenType::Implements => Some("implements"),
            TokenType::Interface => Some("interface"),
            TokenType::Package => Some("package"),
            TokenType::Private => Some("private"),
            TokenType::Protected => Some("protected"),
            TokenType::Public => Some("public"),
            TokenType::Arguments => Some("arguments"),
            TokenType::Eval => Some("eval"),
            TokenType::Debugger => Some("debugger"),
            TokenType::Class => Some("class"),
            TokenType::Extends => Some("extends"),
            TokenType::Constructor => Some("constructor"),
            TokenType::Static => Some("static"),
            TokenType::Get => Some("get"),
            TokenType::Set => Some("set"),
            TokenType::Yield => Some("yield"),
            TokenType::Import => Some("import"),
            TokenType::Export => Some("export"),
            TokenType::From => Some("from"),
            TokenType::As => Some("as"),
            
            // Sentinel
            TokenType::EOF => None,
        }
    }
    
    pub fn to_string(&self) -> String {
        match self {
            TokenType::Identifier(name) => name.clone(),
            TokenType::StringLiteral(s) => format!("\"{}\"", s),
            TokenType::NumberLiteral(n) => n.to_string(),
            TokenType::BigIntLiteral(b) => format!("{}n", b),
            TokenType::RegExpLiteral(pattern, flags) => format!("/{}/{}", pattern, flags),
            TokenType::TemplateLiteral(_) => "`...`".to_string(),
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

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub column: usize,
    pub line: usize,
    pub length: usize,
}

impl Token {
    
    #[inline]
    pub fn new(token_type: TokenType, line: usize, column: usize, length: usize) -> Self {
        Token { token_type, line, column, length }
    }
}
