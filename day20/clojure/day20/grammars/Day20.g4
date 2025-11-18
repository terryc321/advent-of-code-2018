

// Grammar for a simple calculator

grammar Day20;

//entry: START expr END;

// // Rule for an expression
// expr:  | DIR +    // Variable identifier
//        |  expr ALT expr  // Alternation
//        | '(' expr ')'    // Parentheses
//        | expr ( expr * )
//     ;

expr: DIR + ;

// expr: | '(' expr ')'
//       | DIR +
//       | expr '|' expr
//       | expr expr
//       ;


//expr: ID ;

START: '^';

END: '$';

// Token definitions
DIR: 'N' | 'S' | 'E' | 'W' ;

ALT: '|' ;

// Ignore whitespace
WS: [ \t\r\n]+ -> skip;
