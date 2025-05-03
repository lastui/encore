#[derive(Debug, Clone)]
pub struct Module {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    EmptyStatement,
    BlockStatement {
        body: Vec<Statement>,
    },
    ExpressionStatement {
        expression: Expression,
    },
    IfStatement {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>,
    },
    WhileStatement {
        test: Expression,
        body: Box<Statement>,
    },
    ForStatement {
        init: Option<ForInit>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Box<Statement>,
    },
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableKind,
    },
    FunctionDeclaration {
        id: String,
        params: Vec<FunctionParameter>,
        body: Vec<Statement>,
    },
    AsyncFunctionDeclaration {
        id: String,
        params: Vec<FunctionParameter>,
        body: Vec<Statement>,
    },
    ReturnStatement {
        argument: Option<Expression>,
    },
    BreakStatement,
    ContinueStatement,
    TryStatement {
        block: Box<Statement>,
        handler: Option<CatchClause>,
        finalizer: Option<Box<Statement>>,
    },
    ThrowStatement {
        argument: Expression,
    },
    SwitchStatement {
        discriminant: Expression,
        cases: Vec<SwitchCase>,
    },
    ImportDeclaration {
        specifiers: Vec<ImportSpecifier>,
        source: String,
    },
    ExportNamedDeclaration {
        declaration: Option<Box<Statement>>,
        specifiers: Vec<ExportSpecifier>,
        source: Option<String>, // TODO improve
    },
    ExportDefaultDeclaration {
        declaration: Box<Expression>,
    },
    ExportAllDeclaration {
        source: String, // TODO improve
    },
}

#[derive(Debug, Clone)]
pub enum ForInit {
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableKind,
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub id: String,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub is_rest: bool,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub param: Option<String>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum ImportSpecifier {
    ImportSpecifier {
        imported: String,
        local: String,
    },
    ImportDefaultSpecifier(String),
    ImportNamespaceSpecifier(String),
}

#[derive(Debug, Clone)]
pub enum ExportSpecifier {
    ExportSpecifier {
        local: String,
        exported: String,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    ThisExpression,
    NumberLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    UndefinedLiteral,
    ArrayLiteral {
        elements: Vec<Option<Expression>>,
    },
    ObjectLiteral {
        properties: Vec<ObjectProperty>,
    },
    FunctionExpression {
        id: Option<String>,
        params: Vec<FunctionParameter>,
        body: Vec<Statement>,
        is_async: bool,
    },
    ArrowFunctionExpression {
        params: Vec<FunctionParameter>,
        body: ArrowFunctionBody,
        is_async: bool,
    },
    UnaryExpression {
        operator: UnaryOperator,
        argument: Box<Expression>,
    },
    BinaryExpression {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    LogicalExpression {
        left: Box<Expression>,
        operator: LogicalOperator,
        right: Box<Expression>,
    },
    AssignmentExpression {
        left: Box<Expression>,
        operator: AssignmentOperator,
        right: Box<Expression>,
    },
    MemberExpression {
        object: Box<Expression>,
        property: Box<Expression>,
        computed: bool,
    },
    CallExpression {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    NewExpression {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    ConditionalExpression {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum ArrowFunctionBody {
    // TODO either not both
    BlockStatement(Vec<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum ObjectProperty {
    Property {
        key: String,
        value: Expression,
    },
    Shorthand {
        key: String,
        value: Expression,
    },
    Method {
        key: String,
        params: Vec<FunctionParameter>,
        body: Vec<Statement>,
    },
    ComputedProperty {
        key: Expression,
        value: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
    Plus,
    TypeOf,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
}
