use std::fmt;

/// Represents a location in the source code
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub start: [usize; 2], // (line, column)
    pub end: [usize; 2],   // (line, column)
}

/// Represents a JavaScript program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>,
    pub source_type: SourceType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Script,
    Module,
}

/// Represents a JavaScript statement
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement,
    DebuggerStatement,
    WithStatement(WithStatement),
    ReturnStatement(ReturnStatement),
    LabeledStatement(LabeledStatement),
    BreakStatement(BreakStatement),
    ContinueStatement(ContinueStatement),
    IfStatement(IfStatement),
    SwitchStatement(SwitchStatement),
    ThrowStatement(ThrowStatement),
    TryStatement(TryStatement),
    WhileStatement(WhileStatement),
    DoWhileStatement(DoWhileStatement),
    ForStatement(ForStatement),
    ForInStatement(ForInStatement),
    ForOfStatement(ForOfStatement),
    Declaration(Declaration),
}

/// Represents a JavaScript expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    ThisExpression(ThisExpression),
    ArrayExpression(ArrayExpression),
    ObjectExpression(ObjectExpression),
    FunctionExpression(FunctionExpression),
    ArrowFunctionExpression(ArrowFunctionExpression),
    ClassExpression(ClassExpression),
    TaggedTemplateExpression(TaggedTemplateExpression),
    MemberExpression(MemberExpression),
    SuperExpression(SuperExpression),
    MetaProperty(MetaProperty),
    NewExpression(NewExpression),
    CallExpression(CallExpression),
    UpdateExpression(UpdateExpression),
    AwaitExpression(AwaitExpression),
    UnaryExpression(UnaryExpression),
    BinaryExpression(BinaryExpression),
    LogicalExpression(LogicalExpression),
    ConditionalExpression(ConditionalExpression),
    YieldExpression(YieldExpression),
    AssignmentExpression(AssignmentExpression),
    SequenceExpression(SequenceExpression),
}

/// Represents a JavaScript declaration
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    ImportDeclaration(ImportDeclaration),
    ExportNamedDeclaration(ExportNamedDeclaration),
    ExportDefaultDeclaration(ExportDefaultDeclaration),
    ExportAllDeclaration(ExportAllDeclaration),
}

/// Represents a JavaScript pattern (destructuring)
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(Identifier),
    ObjectPattern(ObjectPattern),
    ArrayPattern(ArrayPattern),
    RestElement(RestElement),
    AssignmentPattern(AssignmentPattern),
    MemberExpression(MemberExpression),
}

/// Represents an identifier
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    
    pub name: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrivateIdentifier {
    pub name: Box<str>,
}

/// Represents a literal value
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral(NullLiteral),
    NumericLiteral(NumericLiteral),
    BigIntLiteral(BigIntLiteral),
    RegExpLiteral(RegExpLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NullLiteral {
    
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumericLiteral {
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BigIntLiteral {
    pub value: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegExpLiteral {
    pub pattern: Box<str>,
    pub flags: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WithStatement {
    pub object: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub argument: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LabeledStatement {
    pub label: Identifier,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub label: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub label: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub test: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStatement {
    pub discriminant: Box<Expression>,
    pub cases: Vec<SwitchCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub test: Option<Box<Expression>>,
    pub consequent: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThrowStatement {
    pub argument: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub test: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileStatement {
    pub body: Box<Statement>,
    pub test: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: Option<ForInit>,
    pub test: Option<Box<Expression>>,
    pub update: Option<Box<Expression>>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit {
    VariableDeclaration(VariableDeclaration),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInStatement {
    pub left: ForInOf,
    pub right: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForOfStatement {
    pub left: ForInOf,
    pub right: Box<Expression>,
    pub body: Box<Statement>,
    pub await_token: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInOf {
    VariableDeclaration(VariableDeclaration),
    Pattern(Pattern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThisExpression {
    
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression {
    pub elements: Vec<Option<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectExpression {
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub key: PropertyKey,
    pub value: Box<Expression>,
    pub kind: PropertyKind,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyKey {
    Identifier(Identifier),
    PrivateIdentifier(PrivateIdentifier),
    Literal(Literal),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: BlockStatement,
    pub generator: bool,
    pub async_function: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrowFunctionExpression {
    pub params: Vec<Pattern>,
    pub body: ArrowFunctionBody,
    pub expression: bool,
    pub async_function: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrowFunctionBody {
    BlockStatement(BlockStatement),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: BlockStatement,
    pub generator: bool,
    pub async_function: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclaration {
    pub id: Option<Identifier>,
    pub super_class: Option<Box<Expression>>,
    pub body: ClassBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassExpression {   
    pub id: Option<Identifier>,
    pub super_class: Option<Box<Expression>>,
    pub body: ClassBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassBody {
    pub body: Vec<ClassElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassElement {
    MethodDefinition(MethodDefinition),
    StaticBlock(StaticBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticBlock {
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub key: PropertyKey,
    pub value: FunctionExpression,
    pub kind: MethodKind,
    pub computed: bool,
    pub static_method: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    Constructor,
    Method,
    Get,
    Set,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TaggedTemplateExpression {
    pub tag: Box<Expression>,
    pub quasi: TemplateLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateLiteral {
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateElement {
    pub value: TemplateElementValue,
    pub tail: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateElementValue {
    pub raw: Box<str>,
    pub cooked: Option<Box<str>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: MemberProperty,
    pub computed: bool,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemberProperty {
    Identifier(Identifier),
    Expression(Box<Expression>),
    PrivateIdentifier(PrivateIdentifier), // TODO implement
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuperExpression {
    
}

#[derive(Debug, Clone, PartialEq)]
pub struct MetaProperty {
    pub meta: Identifier,
    pub property: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UpdateExpression {
    pub operator: UpdateOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AwaitExpression {
    pub argument: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Not,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Exponentiation,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    In,
    InstanceOf,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpression {
    pub operator: LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    NullishCoalescing,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpression {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YieldExpression {
    
    pub argument: Option<Box<Expression>>,
    pub delegate: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub operator: AssignmentOperator,
    pub left: AssignmentLeft,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentLeft {
    Pattern(Pattern),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    RemainderAssign,
    ExponentiationAssign,
    LeftShiftAssign,
    RightShiftAssign,
    UnsignedRightShiftAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    BitwiseAndAssign,
    LogicalOrAssign,
    LogicalAndAssign,
    NullishCoalescingAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SequenceExpression {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectPatternProperty {
    Property(ObjectProperty),
    RestElement(RestElement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectProperty {
    pub key: PropertyKey,
    pub value: Pattern,
    pub computed: bool,
    pub shorthand: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayPattern {
    pub elements: Vec<Option<Pattern>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RestElement {
    pub argument: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentPattern {
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDeclaration {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: StringLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSpecifier {
    ImportSpecifier(NamedImportSpecifier),
    ImportDefaultSpecifier(ImportDefaultSpecifier),
    ImportNamespaceSpecifier(ImportNamespaceSpecifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedImportSpecifier {
    pub imported: Identifier,
    pub local: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDefaultSpecifier {
    pub local: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportNamespaceSpecifier {
    pub local: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportNamedDeclaration {
    pub declaration: Option<Box<Declaration>>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<StringLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportSpecifier {
    pub exported: Identifier,
    pub local: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportDefaultDeclaration {
    pub declaration: ExportDefaultDeclarationKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportDefaultDeclarationKind {
    Declaration(Box<Declaration>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportAllDeclaration {
    pub source: StringLiteral,
    pub exported: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub text: String,
    pub multiline: bool,
    pub location: SourceLocation,
}
