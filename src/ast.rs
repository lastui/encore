#[derive(Debug, Clone)]
pub struct Program {
    pub source_type: SourceType,
    pub body: Vec<Statement>,
    pub comments: Vec<Comment>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Script,
    Module,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Expression(Expression),
    If {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>,
    },
    Loop(LoopStatement),
    Declaration(Declaration),
    Return(Option<Expression>),
    Labeled {
        label: Box<str>,
        body: Box<Statement>,
    },
    Break(Option<Box<str>>),
    Continue(Option<Box<str>>),
    Try {
        block: Box<Statement>,
        handler: Option<CatchClause>,
        finalizer: Option<Box<Statement>>,
    },
    Throw(Expression),
    Switch {
        discriminant: Expression,
        cases: Vec<SwitchCase>,
    },
    Import {
        specifiers: Vec<ImportSpecifier>,
        source: Box<str>,
        assertions: Vec<ImportAssertion>,
    },
    Export(ExportDeclaration),
    With {
        object: Expression,
        body: Box<Statement>,
    },
    Debugger,
}

#[derive(Debug, Clone)]
pub enum LoopStatement {
    While {
        test: Expression,
        body: Box<Statement>,
    },
    DoWhile {
        body: Box<Statement>,
        test: Expression,
    },
    For {
        init: Option<ForInit>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Box<Statement>,
    },
    ForIn {
        left: ForInit,
        right: Expression,
        body: Box<Statement>,
    },
    ForOf {
        left: ForInit,
        right: Expression,
        body: Box<Statement>,
        is_await: bool,
    },
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
    Class(ClassDeclaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub id: Box<str>,
    pub params: Vec<Expression>,
    pub body: Vec<Statement>,
    pub is_async: bool,
    pub is_generator: bool,
}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub id: Box<str>,
    pub super_class: Option<Expression>,
    pub body: Vec<ClassMember>,
}

#[derive(Debug, Clone)]
pub enum ClassMember {
    Constructor {
        params: Vec<Expression>,
        body: Vec<Statement>,
    },
    Method {
        key: PropertyKey,
        value: MethodDefinition,
        kind: MethodKind,
        is_static: bool,
    },
    Property {
        key: PropertyKey,
        value: Option<Expression>,
        is_static: bool,
    },
    StaticBlock {
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    pub params: Vec<Expression>,
    pub body: Vec<Statement>,
    pub is_async: bool,
    pub is_generator: bool,
}

#[derive(Debug, Clone)]
pub enum PropertyKey {
    Identifier(Box<str>),
    StringLiteral(Box<str>),
    NumericLiteral(f64),
    Computed(Expression),
    PrivateIdentifier(Box<str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}

#[derive(Debug, Clone)]
pub enum ExportDeclaration {
    Named {
        declaration: Option<Box<Declaration>>,
        specifiers: Vec<ExportSpecifier>,
        source: Option<Box<str>>,
    },
    Default(Box<ExportDefaultDeclaration>),
    All {
        source: Box<str>,
        exported: Option<Box<str>>,
    },
}

#[derive(Debug, Clone)]
pub enum ExportDefaultDeclaration {
    Expression(Expression),
    Function(FunctionDeclaration),
    Class(ClassDeclaration),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Declaration(VariableDeclaration),
    Pattern(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub id: Expression, // TODO maybe tighter
    pub init: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub param: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum ImportSpecifier {
    Named {
        imported: Box<str>,
        local: Box<str>,
    },
    Default(Box<str>),
    Namespace(Box<str>),
}

#[derive(Debug, Clone)]
pub struct ImportAssertion {
    pub key: Box<str>,
    pub value: Box<str>,
}

#[derive(Debug, Clone)]
pub struct ExportSpecifier {
    pub local: Box<str>,
    pub exported: Box<str>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Box<str>),
    This,
    Super,
    Literal(Literal),
    Array(Vec<ArrayElement>),
    Object(Vec<ObjectProperty>),
    Function {
        id: Option<Box<str>>,
        params: Vec<Expression>,
        body: Vec<Statement>,
        is_async: bool,
        is_generator: bool,
    },
    ArrowFunction {
        params: Vec<Expression>,
        body: ArrowFunctionBody,
        is_async: bool,
    },
    Class {
        id: Option<Box<str>>,
        super_class: Option<Box<Expression>>,
        body: Vec<ClassMember>,
    },
    Unary {
        operator: UnaryOperator,
        argument: Box<Expression>,
        prefix: bool,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Logical {
        operator: LogicalOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment {
        operator: AssignmentOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Member {
        object: Box<Expression>,
        property: Box<Expression>,
        computed: bool,
        optional: bool,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Argument>,
        optional: bool,
    },
    New(Box<Expression>),
    Conditional {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Expression>,
    },
    TemplateLiteral {
        quasis: Vec<Box<str>>,
        expressions: Vec<Expression>,
    },
    TaggedTemplate {
        tag: Box<Expression>,
        quasi: Box<Expression>,
    },
    Sequence(Vec<Expression>),
    Spread(Box<Expression>),
    Yield {
        argument: Option<Box<Expression>>,
        delegate: bool,
    },
    Await(Box<Expression>),
    OptionalChain {
        base: Box<Expression>,
        chain: Vec<OptionalChainElement>,
    },
    Import(Box<Expression>),
    MetaProperty {
        meta: Box<str>,
        property: Box<str>,
    },
    PrivateName(Box<str>),
    ChainExpression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum ArrayElement {
    Expression(Expression),
    Spread(Expression),
    Hole,
}

#[derive(Debug, Clone)]
pub enum Argument {
    Expression(Expression),
    Spread(Expression),
}

#[derive(Debug, Clone)]
pub enum OptionalChainElement {
    Property {
        name: Box<str>,
        computed: bool,
    },
    Call {
        arguments: Vec<Argument>,
    },
}

#[derive(Debug, Clone)]
pub enum ArrowFunctionBody {
    Block(Vec<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum ObjectProperty {
    Property {
        key: PropertyKey,
        value: Expression,
        kind: PropertyKind,
        computed: bool,
        shorthand: bool,
    },
    Method {
        key: PropertyKey,
        value: MethodDefinition,
        kind: MethodKind,
        computed: bool,
    },
    Spread(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(Box<str>),
    Boolean(bool),
    Null,
    Undefined,
    RegExp {
        pattern: Box<str>,
        flags: Box<str>,
    },
    BigInt(Box<str>),
}

#[derive(Clone)]
pub struct Comment {
    pub text: Box<str>,
    pub is_block: bool,
    pub span: (u32, u32),
}

impl std::fmt::Debug for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Comment")
            .field("text", &self.text)
            .field("is_block", &self.is_block)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Increment,
    Decrement,
    Not,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    In,
    Of,
    InstanceOf,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    NullishCoalescing,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    ExponentAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
    UnsignedRightShiftAssign,
    LogicalAndAssign,
    LogicalOrAssign,
    NullishAssign,
}
