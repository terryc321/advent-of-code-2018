#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include <cairo/cairo.h>

#include <math.h>
#include <stdio.h>
#include <stddef.h>

#include <libguile.h>

/* should these be static methods ? probably not since using dynamic library */  

 void pixelformat2(SDL_Surface *ptr);
 void output_check();
 void * surface_pixels(SDL_Surface *s);
 int surface_width(SDL_Surface *s);
 int surface_height(SDL_Surface *s);
 int surface_pitch(SDL_Surface *s);
 SDL_PixelFormat *surface_pixelformat(SDL_Surface *s);
uint32_t surface_pixelformat_format(SDL_Surface *s);
void sanity_check_hack(SDL_Window * window , SDL_Renderer *renderer, SDL_Surface *s);

void * surface_pixels(SDL_Surface *s){
  if (s){
    return s->pixels;
  }
  return 0;
}

int surface_pitch(SDL_Surface *s){
  if (s){
    return s->pitch;
  }
  return 0;
}


int surface_width(SDL_Surface *s){
  if (s){
    return s->w;
  }
  return 0;
}

int surface_height(SDL_Surface *s){
  if(s){
    return s->h;
  }
  return 0;
}

SDL_PixelFormat *surface_pixelformat(SDL_Surface *s){
  if(s){
    return (SDL_PixelFormat *)s->format;
  }
  return 0;
}

uint32_t surface_pixelformat_format(SDL_Surface *s){
  if(s){
    SDL_PixelFormat *p = (SDL_PixelFormat *)s->format;
    return p->format;
  }
  return 0;
}



void output_check(){
  printf("this is an output check!\n");
}


void pixelformat2(SDL_Surface *ptr){
  if(ptr){
    char *p = (char *)ptr->format;
    int i = 0 ;
    for (i =0 ; i < 8 ; i ++){
      printf("byte %d is %d\n" , i, *p);
      p ++;	
    }
  }
}

void sanity_check_hack(SDL_Window * window, SDL_Renderer *renderer, SDL_Surface *sdl_surface){

  printf("SDL_Window *window = %p\n", window);
  printf("SDL_Renderer *renderer = %p\n", renderer);
  printf("SDL_Surface *sdl_surface = %p\n", sdl_surface);

  /* return ; // early escape */
  
  
  int window_width;
  int window_height;
  SDL_GetWindowSize(window, &window_width, &window_height);

  printf("window_width=%d\n"
         "window_height=%d\n",
         window_width, window_height);

  int renderer_width;
  int renderer_height;
  SDL_GetRendererOutputSize(renderer, &renderer_width, &renderer_height);

  printf("renderer_width=%d\n"
         "renderer_height=%d\n",
         renderer_width, renderer_height);

  int cairo_x_multiplier = renderer_width / window_width;
  int cairo_y_multiplier = renderer_height / window_height;


  cairo_surface_t *cr_surface = cairo_image_surface_create_for_data((unsigned char *)sdl_surface->pixels,
                                                                       CAIRO_FORMAT_RGB24,
                                                                       sdl_surface->w,
                                                                       sdl_surface->h,
                                                                       sdl_surface->pitch);

  cairo_surface_set_device_scale(cr_surface, cairo_x_multiplier, cairo_y_multiplier);

  cairo_t *cr = cairo_create(cr_surface);

  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  // White background with SDL2 API
  // SDL_FillRect(sdl_surface, NULL, SDL_MapRGB(sdl_surface->format, 255, 255, 255));

  // White background with cairo API
  cairo_set_source_rgba(cr, 1, 1, 1, 1.0);
  cairo_rectangle(cr, 0, 0, 640, 480);
  cairo_fill(cr);

  double xc = 320.0;
  double yc = 240.0;
  double radius = 200.0;
  double angle1 = 45.0  * (M_PI/180.0);
  double angle2 = 180.0 * (M_PI/180.0);

  cairo_set_source_rgba(cr, 0, 0, 0, 1.0);
  cairo_set_line_width(cr, 10.0);
  cairo_arc(cr, xc, yc, radius, angle1, angle2);
  cairo_stroke(cr);

  cairo_set_source_rgba(cr, 1, 0.2, 0.2, 0.6);
  cairo_set_line_width(cr, 6.0);

  cairo_arc(cr, xc, yc, 10.0, 0, 2*M_PI);
  cairo_fill(cr);

  cairo_arc(cr, xc, yc, radius, angle1, angle1);
  cairo_line_to(cr, xc, yc);
  cairo_arc(cr, xc, yc, radius, angle2, angle2);
  cairo_line_to(cr, xc, yc);
  cairo_stroke(cr);

  cairo_surface_flush(cr_surface);
  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);
  
  SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, sdl_surface);
  SDL_FreeSurface(sdl_surface);

  SDL_RenderCopy(renderer, texture, NULL, NULL);

}


/*
try
 (applyStretchedImage surface 1024 768)
 => error
guile: symbol lookup error: ./pixelformat/pixelformat.so: undefined symbol: SDL_UpperBlitScaled


  
int SDL_UpperBlitScaled
    (SDL_Surface * src, const SDL_Rect * srcrect,
    SDL_Surface * dst, SDL_Rect * dstrect);

    
 */


SDL_Surface *applyStretchedImage(SDL_Surface *gScreenSurface, int screenWidth , int screenHeight){
  //Apply the image stretched
  SDL_Rect stretchRect;
  stretchRect.x = 0;
  stretchRect.y = 0;
  stretchRect.w = screenWidth;
  stretchRect.h = screenHeight;
  SDL_Surface* gStretchedSurface = NULL;
  // but this is equivalent to SDL_UpperBlitScaled gStretced NULL gScreen NULL
  // no point in defining rect 
  SDL_BlitScaled( gStretchedSurface, NULL, gScreenSurface, &stretchRect );
  return gStretchedSurface;
}



/*
s is scheme <pointer 0xADDRESS> object
we need c pointer  

SCM pixelformat_wrapper(SCM s){
  SDL_Surface *ptr = (SDL_Surface *)scm_to_pointer(s);
  //printf("pixelformat : ptr->format (%p)\n",ptr->format);
  return scm_from_pointer(ptr->format,NULL);
  //return pointer->scm(ptr->format);
}

void
init_pixelformat (void)
{
  scm_c_define_gsubr ("pixelformat", 1, 0, 0, pixelformat_wrapper);
}

SCM
j0_wrapper (SCM x)
{
  return scm_from_double (j0 (scm_to_double (x, "j0")));
}

void
init_math_bessel (void)
{
  scm_c_define_gsubr ("j0", 1, 0, 0, j0_wrapper);
}
*/

/*
SDL_Texture* loadTexture( char *path , SDL_Renderer *render)
{
    //The final texture
    SDL_Texture* newTexture = NULL;

    //Load image at specified path
    SDL_Surface* loadedSurface = IMG_Load( path );
    if( loadedSurface == NULL )
    {
        printf( "Unable to load image %s! SDL_image Error: %s\n", path, IMG_GetError() );
    }
    else
    {
        //Create texture from surface pixels
        newTexture = SDL_CreateTextureFromSurface( render, loadedSurface );
        if( newTexture == NULL )
        {
            printf( "Unable to create texture from %s! SDL Error: %s\n", path, SDL_GetError() );
        }

        //Get rid of old loaded surface
        SDL_FreeSurface( loadedSurface );
    }

    return newTexture;
}
*/

int size_int(){
  size_t sz = sizeof(int);
  //fprintf(stdout,"size of int is %zu\n",sz);
  return sz;  
}

void show_rect(SDL_Rect *r){
  fprintf(stdout,"rect has x,y,w,h of %d %d %d %d \n" , r->x ,r->y,r->w , r->h);
}
