

// Grammar for a simple calculator

grammar Day20;


expr_ : | word +
      | (expr_ +) '|' (expr_ +)
      | '(' (expr_ *) ')'
;


word:  DIR +
;


//entry: START expr END;

// // Rule for an expression
// expr:  | DIR +    // Variable identifier
//        |  expr ALT expr  // Alternation
//        | '(' expr ')'    // Parentheses
//        | expr ( expr * )
//     ;

// expr : | word 
//        | word (expr *) '|' word (expr *)
//        ;

// term : | word 
//        | '(' term (expr *) ')'
//        | term (expr *) '|' expr 
//        ;


// word: DIR + 
//       ;

//       | DIR (expr2 *) '|' (expr2 *)

// expr2: | DIR (expr *)
//        | '(' expr + ')'
//        | DIR (expr *) '|' (expr *)
//        ;

      

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

// ALT: '|' ;

// Ignore whitespace
WS: [ \t\r\n]+ -> skip;
