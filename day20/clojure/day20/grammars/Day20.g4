

// Grammar for a simple calculator

grammar Day20;

// // Rule for an expression
// expr: expr op=( ALT ) expr  // Alternation
//     | '(' expr ')'       // Parentheses
//     | expr 
//     | ID +                // Variable identifier
//     ;

expr: ID ;

// Token definitions
ID: 'N' | 'S' | 'E' | 'W';

ALT: '|' ;

// Ignore whitespace
WS: [ \t\r\n]+ -> skip;
