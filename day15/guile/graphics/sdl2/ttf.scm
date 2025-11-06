
;; graphics sdl2 ttf

(define-module (graphics sdl2 ttf)
  #:export (
	    ttf-init
	    ttf-quit
	    ttf-render-text-solid
	    ttf-render-utf8-blended-wrapped
	    ttf-open-font
	    ttf-close-font
	    ))

(use-modules (system foreign-library)) ;; probably only need system foreign
(use-modules (system foreign))

;; int TTF_Init(void);
(define ttf-init
  (foreign-library-function "libSDL2_ttf" "TTF_Init"
			    #:return-type int
                            #:arg-types (list)))

;; void TTF_Quit(void);
(define ttf-quit
  (foreign-library-function "libSDL2_ttf" "TTF_Quit"
			    #:return-type void
                            #:arg-types (list)))

;; SDL_Surface * TTF_RenderText_Solid(TTF_Font *font,   const char *text, SDL_Color fg);
;; what do if structure was just as a parameter , not a pointer , how cope if structure bigger than 64bit ?
;; does scheme do the integer conversion implicitly then ?
(define ttf-render-text-solid
  (foreign-library-function "libSDL2_ttf" "TTF_RenderText_Solid"
			    #:return-type '*
                            #:arg-types (list '* '* uint32)))



;; typedef struct SDL_Color
;; {
;;     Uint8 r;
;;     Uint8 g;
;;     Uint8 b;
;;     Uint8 a;
;; } SDL_Color;
;; SDL_Color is just an unsigned 32 bit uint32  (4 * 8 = 32)

;;SDL_Surface * TTF_RenderUTF8_Blended_Wrapped(TTF_Font *font, const char *text, SDL_Color fg, Uint32 wrapLength);
(define ttf-render-utf8-blended-wrapped
  (foreign-library-function "libSDL2_ttf" "TTF_RenderUTF8_Blended_Wrapped"
			    #:return-type '*
                            #:arg-types (list '* '* uint32 uint32)))



;; gFont = TTF_OpenFont( "16_true_type_fonts/lazy.ttf", 28 );
;; TTF_Font * TTF_OpenFont(const char *file, int ptsize);
(define ttf-open-font
  (foreign-library-function "libSDL2_ttf" "TTF_OpenFont"
			    #:return-type '*
                            #:arg-types (list '* int)))

;; void TTF_CloseFont(TTF_Font *font);
(define ttf-close-font
  (foreign-library-function "libSDL2_ttf" "TTF_CloseFont"
			    #:return-type void
                            #:arg-types (list '*)))

