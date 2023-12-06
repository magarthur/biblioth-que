#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "colors.h"


double map(int,int,int,double,double);
void render_init();
void render_image();
void get_pixel_color();
void save_image();
void cam2react();
void modifier_nom();
int m2v();



#define DEFAULT_WIDTH 1000
#define DEFAULT_HEIGHT 1000
#define STRMAX 256
#define BW 0
#define BW_ALTERN 1
#define GREY_ST 2
#define GREY_SM 3
#define RGB 4
#define MAX_COLOR 255

struct pixdiv
{
   int iter;
   int x;
   int y;
};


struct camera
{
   double x;
   double y;
   double width;
   double height;
};

struct render 
{
    int W;                        /* largeur de l'image */
    int H;                        /* Hauteur de l'image */
    int max_iter;
    struct pixdiv *image;
    double xmin;                 /* abscisse min dans le plan complexe */
    double xmax;                 /* abscisse max dans le plan complexe */
    double ymin;                 /* ordonée min dans le plan complexe */
    double ymax;                 /* ordonée max dans le plan complexe */
    double radius;
    char basename[STRMAX];
    struct camera pov;
    int type;                    /* Pour le choix du style de l'image */
};




int main(int argc, char *argv[])
{
    struct render set;
    render_init(&set,argc, argv);
    cam2react(&set,&(set.pov));
    render_image(&set);
    save_image(&set);
    free(set.image);
   
   return 0;
}



double map(int v, int imin, int imax, double omin, double omax)                  /* fonction qui pour une valeur v dans l'intervalle [imin,imax] retourne la valeur propotionelle res dans l'intervalle [omin,omax] */
{
   double p;
   double res;
   p = (omax - omin)/(imax - imin);
   res = omin + (v-imin)*p;
   return res;
}


void render_init(struct render *set, int argc, char *argv[])                                             /* initialisation de la structure set */
{
   char nom[STRMAX-5];
   set->pov.x = -1.142817421949293;
   set->pov.y = -0.21198254168631775;
   set->pov.width = 0.0055;
   set->pov.height = 0.0055;
   set->W = DEFAULT_WIDTH;
   set->H = DEFAULT_HEIGHT;
   set->max_iter = 100;
   set->radius = 2;
   set->type = GREY_ST;
   

   if (argc == 1)                                                                   /* les différents arguments de la fonction main */
   {
      printf("Vous avez chosit l'option valeurs par défauts. \n");
      exit(EXIT_FAILURE);
   }
   if (argc >=2)
   {
      if (sscanf(argv[1],"%d",&(set->max_iter)) !=1 )
      {
         printf("La valeur demandé est un entier");
         exit(EXIT_FAILURE);
      }
   }
   if(argc >= 3)
   {
      if (sscanf(argv[2],"%lf,%lf,%lf,%lf",&(set->pov.x),&(set->pov.y),&(set->pov.width),&(set->pov.height)) != 4)
      {
         printf("Les valeurs");
         exit(EXIT_FAILURE);
      }
   }
   if(argc >=4)
   {
      if (sscanf(argv[3],"%dx%d",&(set->W),&(set->H)) != 2)
      {
         printf("erreur");
         exit(EXIT_FAILURE);
      }
   }
   if (argc >=5)
   {
      if (sscanf(argv[4],"%[^\n]",nom) != 1)
      {
         exit(EXIT_FAILURE);
      }
   }
   modifier_nom(nom);
   strncpy(set->basename, nom, STRMAX-5);
   set->image = (struct pixdiv *)malloc(set->W * set->H * sizeof(struct pixdiv));
   if (set->image == NULL) 
   {
      exit(EXIT_FAILURE);
   }
}


void render_image(struct render *set)                                      /* instructions de calcul de l’image */
{

    int iter;
    int i;
    int j;
    int k;
    double x0;
    double y0;
    double x;
    double y;
    double nx;

    for (j = 0; (j <set->H); j++) /* parcours de la grille de pixels de l'image */
   {
      printf("la ligne en cours de calcul est %d \r",j);
      for (i = 0; (i <set->W); i++)
      {
         x = map(i,0,set->W,set->xmin,set->xmax);
         x0=x;
         y = map(j,0,set->H,set->ymin,set->ymax);
         y0=y;
         iter = 0;
         while(((x*x +y*y)<=set->radius*set->radius) && (iter<set->max_iter))
         {
            nx = x;
            x= x*x - (y*y) + x0;
            y = 2*nx*y +y0;
            iter = iter +1;
         }
         for (k = 0; k < 4; ++k)                         /* afin d'améliorer la précision, on augmente le nombre d'itérations. Ces itérations ne modifient que les champs x et y */
         {
            nx = x;
            x= x*x - (y*y) + x0;
            y = 2*nx*y +y0;
         }
         set->image[m2v(set,i,j)].iter = iter; 
         set->image[m2v(set,i,j)].x = x; 
         set->image[m2v(set,i,j)].y = y; 
      }
   }
}


void get_pixel_color(struct render *set, int Px, int Py, int *r, int *g, int *b)       /* Fonction qui, pour le pixel (Px,Py) , retourne sa couleur via ses paramètres *r, *g, *b.  pour différents cas choisi préalablement à partir de set->type*/
{
   int n_it;
   double x;
   double y;
   double grey;
   struct color hsv;
   struct color rgb;

   n_it = set->image[m2v(set,Px,Py)].iter;
   x = set->image[m2v(set,Px,Py)].x;
   y = set->image[m2v(set,Px,Py)].y;

   switch(set->type)
   {
      case BW:                                                          /* noir et blanc */
         if (n_it == set->max_iter)
         {
            *r = 0;
            *g = 0;
            *b = 0;
         }
         else
         {
            *r = 255;
            *g = 255;
            *b = 255;
         }
         break;

      case BW_ALTERN:
         if (n_it == set->max_iter || set->max_iter % 2 == n_it)     /* noir et blanc alterné */
         {
            *r = 0;
            *g = 0;
            *b = 0;
         }
         else
         {
            *r = 255;
            *g = 255;
            *b = 255;
         }
         break;

      case GREY_ST:                                                  /* 256 nuances de gris */
         if (n_it == set->max_iter)
         {
            *r = 0;
            *g = 0;
            *b = 0;
         }
         else
         {
            *r = (n_it*MAX_COLOR)/set->max_iter;
            *g = (n_it*MAX_COLOR)/set->max_iter;
            *b = (n_it*MAX_COLOR)/set->max_iter;
         }
         break;

      case GREY_SM:
         if(n_it == set->max_iter)
         {
            *r = 0;
            *g = 0;
            *b = 0;
         }
         else
         {
            grey = 5 + n_it - log(log(x*x + y*y)/log(2))/log(2);
            grey = floor(512*grey/set->max_iter);
            if(grey>255)
            {
               grey = 255;
            }

             *r = grey;
             *g = grey;
             *b = grey;
         }
         break;

      case RGB:                                                   /* en couleur */

         grey = 5 + n_it - log(log(x*x + y*y)/log(2))/log(2);
         grey = floor(512*grey/set->max_iter);
         if(grey>255)
         {
            grey = 255;
         }
         hsv.c1 = 360*grey/set->max_iter;
         hsv.c2 = 1.0;
         hsv.c3 = 1.0;
         hsv2rgb(&rgb,&hsv);
         *r = rgb.c3;
         *g = rgb.c2;
         *b = rgb.c1;
         break;
   }  
}


void save_image(struct render *set)                               /* instructions nécessaires à l’écriture des données de l’image dans le fichier.*/
{

    int i;
    int j;
    int r,g,b;
    FILE *fout;
    char name[STRMAX];

    strncpy(name,set->basename,STRMAX);
    strcat(name,".ppm");
    fout = fopen(name,"w");

    fprintf(fout,"P3\n");
    fprintf(fout,"#le nombre max d'itération est : %d",set->max_iter);
    fprintf(fout,"#la zone du plan complexe visualisée est xmin = -2, xmax = 0.48, ymin = -1.24, ymax = 1.24 \n");
    fprintf(fout,"%d %d\n", set->W,set->H);
    fprintf(fout,"%d\n",255);
    for (j = 0; (j <set->H); j++) /* parcours de la grille de pixels de l'image */
    {
       for (i = 0; (i < set->W); i++)
       {
          get_pixel_color(set,i,j,&r,&g,&b);

          fprintf(fout,"%d %d %d",r,g,b);
          fprintf(fout,"\n");
       }

    }
    fclose(fout);
}


int m2v(struct render *set, int x, int y)
{
   return y*set->W +x;                       /* indice pour passer d'un tableau 2D vers 1 tableau 1D */
}


void cam2react(struct render *set, struct camera *pov)     /* initialisation des champs xmin, xmax, ymin, ymax de la struct render  */
{
   set->xmin = pov->x - pov->width/2;
   set->xmax= pov->x  + pov->width/2;
   set->ymin= pov->y - pov->height/2;
   set->ymax= pov->y + pov->height/2;
}

void modifier_nom(char *nom)                             /* fonction permettant de supprimer les éventuelles séquences d’espaces en début et en fin de chaîne. Les éventuelles séquences d’espaces séparant les mots seront  remplacées par un unique caractère _ */
{
    int i, j = 0;
    int debutMot = 0; 

    for (i = 0; nom[i] != '\0'; i++) 
    {
        if (nom[i] != ' ') 
        {
            if (!debutMot) 
            {
                if (j > 0 && nom[j - 1] != '_') 
                {
                    nom[j++] = '_';
                }
                debutMot = 1; 
            }
            nom[j++] = nom[i];
        } 
        else 
        {
         debutMot = 0;
        }
    }
    while (j > 0 && nom[j - 1] == ' ') 
    {
        j--;
    }

    nom[j] = '\0';
}
