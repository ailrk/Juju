%token T_LParen
%token T_RParen
%token T_BSlash
%token T_Dot
%token T_Unit
%token T_Let
%token T_In
%token T_Eq
%token T_EOF
%token <string> T_Ident


%start <Expr.expr> topexpr
%%

topexpr : e=expr; T_EOF { e }

expr : t=app_expr { t }
     | T_BSlash; l=abs_expr { l }
     | T_Let; x=T_Ident; T_Eq; e1=term; T_In; e2=term { Expr.Let(x, e1, e2) }
     ;

app_expr : t=term { t }
         | f=app_expr; x=term { Expr.App(f, x) }
         ;

abs_expr : x=T_Ident; T_Dot;  t=term { Expr.abs_expr(x, t) }
         | x=T_Ident; l=abs_expr { Expr.abs_expr(x, l) }
         ;
