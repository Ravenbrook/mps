# define size     511
# define classMax 3
# define typeMax  12
# define d	  8
# define true     1
# define false    0

static int class[typeMax+1];
static int pieceCount[classMax+1];
static int pieceMax[typeMax+1];
static int puzzle[size+1];
static int p[typeMax+1][size+1];
static int kount,clock;


fit(i,j)
int i,j;
  {
    register k,plim;
    plim = pieceMax[i];
    for (k=0; k <= plim; k++)
      {
        if (p[i][k])
	  {
            if (puzzle[j+k]) return (false);
	  }
       }
    return(true);
  }

int place (i,j)
int i,j;
  {
    register k,plim;
    plim = pieceMax[i];
    for (k = 0; k <= plim; k++)
      if (p[i][k]) puzzle[j+k] = true;
    pieceCount[class[i]] = pieceCount[class[i]] - 1;
    for (k=j; k <= size; k++)
      if (!puzzle[k])
	{
          return(k);
	}
    printf("Puzzle filled.\n");
    return(0);
  }

remove(i,j)
int i,j;
  {
    register k,plim;   
    plim = pieceMax[i];
    for (k=0; k<=plim; k++)
      if (p[i][k]) puzzle[j+k] = false;
    pieceCount[class[i]] = pieceCount[class[i]] + 1;
  }

trial(j)
int j;
  {
    register i,k;
    for (i = 0; i <= typeMax; i++)
      if (pieceCount[class[i]] != 0) 
        if (fit (i, j))
	  {
            k = place (i, j);
            if (trial(k)||(k == 0))
	      {                                         
/*	        printf("piece %d at %d\n",i+1,k+1); */
		kount = kount + 1;
		return(true);
	      }
            else remove (i, j);
          }
    kount = kount + 1;
    return(false);
  }

main()
  {
    register i,j,k,m,n;

/*    printf("starting\n"); */

    for (m = 0; m<=size; m++)
      puzzle[m] = true;

    for (i=1; i <= 5; i++)
      for (j=1; j<=5; j++)
	for (k=1; k<=5 ; k++)
          puzzle[i+d*(j+d*k)] = false;

    for (i=0;i<=typeMax;i++)
      for (j=0;j<=size;j++)
        p[i][j] = false;

    for (i=0;i<=3;i++)
      for (j=0;j<=1;j++)
	for (k = 0;k<=0;k++)
          p[0][i+d*(j+d*k)] = true;

    class[0] = 0;
    pieceMax[0] = 3+d*1+d*d*0;

    for (i=0;i<=1;i++) 
      for (j=0;j<=0;j++)
	for (k=0;k<=3;k++)
          p[1][i+d*(j+d*k)] = true;

    class[1] = 0;
    pieceMax[1] = 1+d*0+d*d*3;

    for (i=0;i<=0;i++) 
      for (j=0;j<=3;j++)
	for (k=0;k<=1;k++)
          p[2][i+d*(j+d*k)] = true;

    class[2] = 0;
    pieceMax[2] = 0+d*3+d*d*1;

    for (i=0;i<=1;i++) 
      for (j=0;j<=3;j++)
	for (k=0;k<=0;k++)
          p[3][i+d*(j+d*k)] = true;

    class[3] = 0;
    pieceMax[3] = 1+d*3+d*d*0;

    for (i=0;i<=3;i++) 
      for (j=0;j<=0;j++)
	for (k=0;k<=1;k++)
          p[4][i+d*(j+d*k)] = true;

    class[4] = 0;
    pieceMax[4] = 3+d*0+d*d*1;

    for (i=0;i<=0;i++) 
      for (j=0;j<=1;j++)
	for (k=0;k<=3;k++)
          p[5][i+d*(j+d*k)] = true;

    class[5] = 0;
    pieceMax[5] = 0+d*1+d*d*3;

    for (i=0;i<=2;i++) 
      for (j=0;j<=0;j++)
	for (k=0;k<=0;k++)
          p[6][i+d*(j+d*k)] = true;

    class[6] = 1;
    pieceMax[6] = 2+d*0+d*d*0;

    for (i=0;i<=0;i++) 
      for (j=0;j<=2;j++)
	for (k=0;k<=0;k++)
          p[7][i+d*(j+d*k)] = true;

    class[7] = 1;
    pieceMax[7] = 0+d*2+d*d*0;

    for (i=0;i<=0;i++) 
      for (j=0;j<=0;j++)
	for (k=0;k<=2;k++)
          p[8][i+d*(j+d*k)] = true;

    class[8] = 1;
    pieceMax[8] = 0+d*0+d*d*2;

    for (i=0;i<=1;i++) 
      for (j=0;j<=1;j++)
	for (k=0;k<=0;k++)
          p[9][i+d*(j+d*k)] = true;

    class[9] = 2;
    pieceMax[9] = 1+d*1+d*d*0;

    for (i=0;i<=1;i++) 
      for (j=0;j<=0;j++)
	for (k=0;k<=1;k++)
          p[10][i+d*(j+d*k)] = true;

    class[10] = 2;
    pieceMax[10] = 1+d*0+d*d*1;

    for (i=0;i<=0;i++) 
      for (j=0;j<=1;j++)
	for (k=0;k<=1;k++)
          p[11][i+d*(j+d*k)] = true;

    class[11] = 2;
    pieceMax[11] = 0+d*1+d*d*1;

    for (i=0;i<=1;i++) 
      for (j=0;j<=1;j++)
	for (k=0;k<=1;k++)
          p[12][i+d*(j+d*k)] = true;

    class[12] = 3;
    pieceMax[12] = 1+d*1+d*d*1;
    pieceCount[0] = 13;
    pieceCount[1] = 3;
    pieceCount[2] = 1;
    pieceCount[3] = 1;
    m = 1+d*(1+d*1);
    kount = 0;

    if (fit(0, m)) 
      {
        n = place(0, m) ;
      }
    else 
      {
	printf("error 1\n");
      }

/*    printf("n = %d n\n",n); */

    if (trial(n)) 
      {
	printf("success in %d trials\n", kount);
      }
    else 
      {
	printf("failure\n");
      }

/*    printf("elapsed user time:\n"); */
  }


