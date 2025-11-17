

/*

typical input

x=581, y=396..399
y=1491, x=566..573
y=1470, x=599..601
y=980, x=526..541
y=1173, x=487..497
y=402, x=616..623
y=361, x=534..577
x=461, y=1151..1163

INT DASH INT WS ALPHA COLON WS ALPHAS

int - int char: chars+ 

*/
grammar Day17;


top:
  ( 'x=' INT ',' WS+ 'y=' INT DOT DOT INT )
  | ( 'y=' INT ',' WS+ 'x=' INT DOT DOT INT ) ;

DASH: '-' ;

INT: [0-9] + ; 

COLON: ':' ;

DOT: '.' ;

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
