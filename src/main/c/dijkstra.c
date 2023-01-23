#include <stdio.h>


  // the graph is stored as an adjecency list
  // where edges are grouped by start vertex
  //const int n=6;  // number of vertices
  //const int m=9;  // number of edges

  // number of vertices
#define n 6
  // number of edges
#define m 9
  // start vertex of edge, grouped by vertex
  int node1[]={0, 0, 0, 1, 1, 2, 2, 3, 4};
  // end vertex of edge
  int node2[]={1, 2, 5, 2, 3, 3, 5, 4, 5};
  // weight of edge
  int dist[]={7, 9, 14, 10, 15, 11, 2, 6, 9};
  // vertex to edge index link
  int edge[]={0, 3, 5, 7, 8, 0};

  // Data structures for Dijkstras algorithm
  //int done[]= new int[n]; // 1 bit
  //int prev=new int[n];
  //int wght[]=new int[n];
   int done[n];
   int prev[n];
   int wght[n];

  const int MAX=1000000;

  /* Function to print an array */
  void printArray(int arr[], int size){
    int i;
    for (i = 0; i < size; i++)
      printf("%d ", arr[i]);
    printf("\n");
  }

  void dijkstra(int start){
    for(int i=0;i<n;i++){done[i]=0; prev[i]=-1; wght[i]=MAX;}
    wght[start]=0;
    int cur=-1, w = 0;
    for(int k=0;k<n;k++) {
      cur = -1; w = MAX;
      for (int i = 0; i < n; i++) {
        if (done[i]==0 && wght[i] < w) {
          cur = i;
          w = wght[i];
        }
      }
      printf("cur %d\n",cur);
      printArray(prev,n);
      printArray(wght,n);
      if(cur<0)break;
      int j = edge[cur];
      while (j < m) {
        int n1 = node1[j], n2 = node2[j], d = dist[j];
        if (n1 != cur) break;
        int d2 = wght[n2], d3 = w + d;
        if (d2 > d3) {
          prev[n2] = cur;
          wght[n2] = d3;
        }
        j++;
      }
      done[cur] = 1;
    }
    printArray(prev,n);
    printArray(wght,n);
    //
    // print route from vertex n-1 to 0
    cur = n-1;
    while(cur>0){
      printf("vertex %d %d\n",cur,wght[cur]);
      cur = prev[cur];
    }
  }




  // Driver program to test above functions
  int main()
  {
      dijkstra(0);
      printArray(prev, n);
      printArray(wght, n);
      return 0;
  }

