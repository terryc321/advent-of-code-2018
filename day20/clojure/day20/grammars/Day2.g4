

/*

int - int char: chars+ 

*/
grammar Day2;

/* dash INT WS ALPHA COLON WS alphas */
top:
 INT DASH INT WS ALPHA COLON WS ALPHAS
;

DASH: '-' ;

INT: [0-9] + ; 

COLON: ':' ;

dash: '-' ;

ALPHA:  [a-z] ;

ALPHAS: [a-z] + ;

WS : ' ' ;


/*
WS
  : ( ' '
  | '\t'
  | '\n'
  | '\r'
  ) -> channel(HIDDEN)
;
*/
