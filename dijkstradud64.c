#include <stdio.h>
#include <time.h>

  // the graph is stored as an adjecency list
  // where edges are grouped by start vertex
  //const int n=6;  // number of vertices
  //const int m=9;  // number of edges

  // number of vertices

  // number of edges

  // start vertex of edge, grouped by vertex
  // end vertex of edge
  // weight of edge
  // vertex to edge index link


//  String g="src\\d-ud-64-v.csv";
#define n  64
//  int n = 64;
#define m  2124
//  int m = 2124;
  int node1[] = {
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63
  };
  int node2[] = {
      1, 11, 12, 13, 14, 17, 18, 2, 21, 25, 26, 28, 29, 32, 37, 39, 4, 40, 42, 43, 44, 45, 46, 48, 49, 50, 51, 52, 56, 58, 59, 6, 61, 62, 8, 9, 0, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 26, 28, 3, 31, 32, 34, 36, 37, 39, 40, 41, 43, 44, 46, 47, 48, 5, 50, 51, 53, 54, 58, 60, 61, 0, 10, 11, 12, 15, 18, 19, 20, 21, 24, 25, 26, 31, 34, 35, 38, 40, 42, 43, 47, 50, 51, 52, 54, 55, 57, 58, 59, 62, 7, 9, 1, 10, 11, 13, 14, 17, 20, 22, 23, 24, 26, 27, 28, 29, 30, 33, 37, 41, 42, 43, 46, 47, 48, 51, 52, 54, 56, 57, 58, 59, 60, 61, 62, 7, 8, 0, 10, 11, 13, 15, 16, 17, 18, 21, 22, 23, 27, 28, 30, 31, 32, 33, 34, 36, 37, 40, 41, 46, 47, 48, 49, 52, 54, 59, 6, 61, 1, 10, 11, 15, 17, 19, 21, 23, 25, 26, 27, 30, 34, 35, 36, 38, 40, 41, 42, 49, 50, 52, 56, 57, 58, 59, 6, 60, 61, 9, 0, 10, 11, 12, 13, 16, 17, 19, 20, 24, 25, 28, 29, 30, 35, 36, 37, 38, 39, 4, 42, 44, 46, 47, 48, 49, 5, 50, 55, 56, 57, 58, 59, 63, 7, 8, 9, 10, 14, 16, 18, 19, 2, 21, 22, 24, 26, 27, 28, 29, 3, 33, 34, 35, 37, 39, 40, 42, 44, 45, 49, 50, 53, 54, 55, 56, 58, 59, 6, 60, 8, 9, 0, 10, 13, 14, 16, 17, 18, 21, 24, 25, 27, 28, 3, 30, 32, 33, 34, 35, 36, 37, 38, 43, 44, 48, 49, 51, 55, 6, 60, 61, 7, 0, 11, 12, 15, 18, 19, 2, 21, 26, 27, 29, 34, 38, 40, 41, 44, 45, 46, 47, 5, 50, 51, 53, 55, 56, 57, 58, 59, 6, 60, 63, 7, 11, 13, 15, 16, 17, 18, 19, 2, 20, 23, 24, 27, 28, 3, 30, 31, 33, 35, 36, 37, 39, 4, 40, 42, 43, 45, 46, 47, 5, 55, 56, 58, 6, 7, 8, 0, 10, 15, 17, 18, 19, 2, 20, 22, 24, 26, 27, 28, 3, 32, 33, 35, 36, 37, 4, 40, 45, 49, 5, 51, 55, 56, 58, 59, 6, 63, 9, 0, 13, 15, 17, 18, 2, 20, 21, 22, 23, 24, 25, 27, 29, 30, 31, 34, 35, 37, 39, 40, 41, 42, 44, 47, 48, 49, 51, 57, 6, 60, 63, 9, 0, 1, 10, 12, 14, 19, 21, 23, 26, 28, 3, 31, 33, 34, 4, 40, 43, 44, 46, 48, 49, 50, 51, 52, 54, 56, 57, 59, 6, 60, 61, 62, 63, 8, 0, 1, 13, 17, 18, 19, 22, 24, 28, 3, 32, 33, 35, 38, 39, 40, 42, 45, 46, 50, 52, 53, 54, 55, 59, 63, 7, 8, 1, 10, 11, 12, 17, 18, 19, 2, 20, 21, 23, 24, 25, 26, 28, 29, 31, 32, 33, 35, 38, 39, 4, 42, 46, 49, 5, 54, 61, 62, 63, 9, 1, 10, 17, 19, 25, 26, 27, 28, 29, 30, 31, 32, 35, 36, 37, 4, 40, 43, 46, 48, 49, 51, 54, 55, 56, 58, 6, 60, 62, 63, 7, 8, 0, 1, 10, 11, 12, 14, 15, 16, 18, 19, 20, 22, 24, 26, 28, 3, 30, 31, 34, 36, 37, 4, 44, 48, 49, 5, 50, 51, 54, 55, 57, 58, 59, 6, 61, 62, 63, 8, 0, 1, 10, 11, 12, 14, 15, 17, 2, 21, 23, 24, 27, 28, 29, 32, 33, 37, 4, 40, 41, 43, 45, 47, 48, 49, 54, 55, 56, 58, 61, 7, 8, 9, 1, 10, 11, 13, 14, 15, 16, 17, 2, 20, 22, 24, 26, 31, 34, 35, 39, 41, 42, 43, 47, 48, 49, 5, 51, 54, 55, 56, 57, 6, 60, 61, 62, 63, 7, 9, 1, 10, 11, 12, 15, 17, 19, 2, 22, 24, 25, 3, 30, 33, 34, 35, 37, 38, 39, 40, 42, 44, 45, 46, 47, 49, 50, 52, 55, 57, 58, 6, 60, 61, 62, 0, 1, 12, 13, 15, 18, 2, 22, 24, 26, 27, 28, 37, 38, 39, 4, 40, 42, 43, 45, 46, 47, 48, 5, 52, 53, 54, 56, 58, 59, 61, 62, 63, 7, 8, 9, 11, 12, 14, 17, 19, 20, 21, 28, 29, 3, 31, 32, 33, 36, 37, 38, 4, 41, 42, 43, 44, 46, 47, 53, 54, 55, 56, 59, 7, 1, 10, 12, 13, 15, 18, 24, 25, 28, 3, 30, 31, 32, 33, 34, 37, 38, 39, 4, 41, 42, 43, 45, 46, 47, 49, 5, 53, 55, 57, 58, 60, 62, 1, 10, 11, 12, 14, 15, 17, 18, 19, 2, 20, 21, 23, 25, 27, 3, 30, 31, 32, 33, 35, 38, 39, 43, 44, 47, 48, 49, 50, 51, 53, 54, 56, 57, 58, 6, 61, 7, 8, 0, 12, 15, 16, 2, 20, 23, 24, 27, 28, 30, 31, 36, 37, 41, 42, 45, 47, 48, 49, 5, 53, 54, 55, 57, 58, 6, 62, 63, 8, 0, 1, 11, 13, 15, 16, 17, 19, 2, 21, 28, 3, 30, 32, 36, 37, 39, 42, 43, 44, 45, 46, 49, 5, 51, 52, 54, 56, 58, 61, 7, 9, 10, 11, 12, 16, 18, 21, 24, 25, 28, 29, 3, 31, 32, 34, 35, 36, 39, 4, 40, 41, 42, 44, 48, 49, 5, 53, 54, 55, 56, 57, 58, 59, 61, 63, 7, 8, 9, 0, 1, 10, 11, 13, 14, 15, 16, 17, 18, 21, 22, 23, 25, 26, 27, 3, 32, 34, 35, 36, 39, 4, 41, 42, 44, 48, 49, 51, 53, 57, 59, 6, 60, 61, 7, 8, 0, 12, 15, 16, 18, 22, 27, 3, 30, 31, 33, 34, 37, 39, 41, 42, 44, 45, 48, 51, 52, 56, 58, 59, 6, 60, 61, 63, 7, 9, 10, 12, 16, 17, 20, 23, 24, 25, 26, 29, 3, 31, 33, 38, 4, 40, 41, 42, 46, 48, 49, 5, 50, 51, 54, 57, 58, 59, 6, 61, 63, 8, 1, 10, 12, 13, 15, 16, 17, 19, 2, 22, 23, 24, 25, 27, 29, 30, 33, 34, 35, 36, 39, 4, 46, 47, 51, 52, 53, 54, 56, 57, 60, 61, 62, 63, 0, 1, 11, 14, 15, 16, 18, 22, 23, 24, 26, 27, 28, 33, 34, 39, 4, 40, 41, 42, 43, 45, 46, 48, 50, 53, 54, 55, 56, 59, 62, 8, 10, 11, 13, 14, 15, 18, 20, 22, 23, 24, 29, 3, 30, 31, 32, 35, 36, 38, 4, 40, 42, 44, 45, 46, 47, 48, 49, 50, 54, 55, 56, 57, 60, 7, 8, 1, 12, 13, 17, 19, 2, 20, 23, 27, 28, 29, 31, 32, 38, 4, 45, 46, 47, 48, 49, 5, 50, 51, 52, 53, 55, 57, 60, 61, 62, 7, 8, 9, 10, 11, 12, 14, 15, 16, 19, 2, 20, 24, 27, 28, 31, 33, 36, 39, 40, 41, 42, 47, 48, 5, 51, 52, 53, 54, 55, 58, 6, 60, 7, 8, 1, 10, 11, 16, 17, 22, 25, 26, 27, 28, 31, 33, 35, 37, 4, 40, 41, 43, 44, 45, 48, 5, 52, 55, 6, 60, 63, 8, 0, 1, 10, 11, 12, 16, 17, 18, 20, 21, 22, 23, 25, 26, 29, 3, 36, 39, 4, 41, 44, 45, 47, 49, 54, 55, 57, 6, 60, 62, 63, 7, 8, 14, 15, 2, 20, 21, 22, 23, 24, 30, 33, 34, 43, 47, 48, 49, 5, 52, 53, 54, 56, 57, 6, 60, 61, 62, 63, 8, 9, 0, 1, 10, 12, 14, 15, 19, 20, 21, 23, 24, 26, 27, 28, 29, 31, 32, 35, 37, 40, 41, 44, 46, 47, 48, 50, 52, 56, 58, 6, 60, 61, 63, 7, 0, 1, 10, 11, 12, 13, 14, 16, 18, 2, 20, 21, 27, 30, 32, 33, 35, 36, 39, 4, 42, 45, 49, 5, 50, 54, 55, 57, 58, 62, 63, 7, 9, 1, 12, 18, 19, 22, 23, 25, 27, 28, 29, 3, 30, 32, 35, 36, 37, 39, 4, 42, 43, 44, 47, 48, 49, 5, 52, 54, 55, 56, 61, 9, 0, 10, 12, 14, 15, 19, 2, 20, 21, 22, 23, 25, 26, 27, 28, 29, 3, 30, 32, 33, 35, 40, 41, 43, 45, 47, 48, 49, 5, 50, 51, 53, 54, 56, 58, 6, 60, 62, 63, 7, 0, 1, 10, 13, 16, 18, 19, 2, 21, 22, 23, 24, 26, 3, 32, 36, 38, 41, 42, 44, 45, 47, 48, 50, 51, 52, 55, 57, 58, 59, 60, 8, 0, 1, 12, 13, 17, 20, 22, 24, 26, 27, 28, 29, 33, 36, 37, 39, 41, 43, 47, 48, 51, 52, 53, 54, 6, 61, 7, 8, 9, 0, 10, 11, 14, 18, 20, 21, 23, 25, 26, 29, 32, 33, 34, 36, 37, 40, 42, 43, 46, 50, 52, 54, 56, 61, 7, 9, 0, 1, 10, 13, 14, 15, 16, 20, 21, 22, 23, 26, 3, 30, 31, 32, 33, 34, 39, 4, 45, 49, 50, 52, 55, 56, 58, 6, 63, 9, 1, 10, 12, 18, 19, 2, 20, 21, 22, 23, 24, 25, 3, 31, 33, 34, 35, 37, 38, 39, 4, 41, 42, 43, 44, 48, 50, 51, 53, 54, 55, 57, 59, 6, 61, 9, 0, 1, 12, 13, 16, 17, 18, 19, 21, 24, 25, 27, 28, 29, 3, 30, 32, 33, 34, 35, 36, 38, 39, 4, 41, 42, 43, 44, 47, 50, 51, 52, 55, 6, 62, 8, 0, 11, 12, 13, 15, 16, 17, 18, 19, 20, 23, 24, 25, 26, 27, 28, 30, 33, 34, 37, 38, 4, 40, 41, 42, 46, 5, 52, 54, 55, 6, 60, 61, 62, 63, 7, 8, 0, 1, 13, 14, 17, 2, 20, 24, 30, 32, 33, 34, 39, 40, 42, 43, 45, 46, 47, 48, 5, 52, 54, 55, 56, 57, 58, 59, 6, 62, 63, 7, 9, 0, 1, 11, 12, 13, 16, 17, 19, 2, 24, 26, 28, 29, 3, 30, 31, 34, 35, 42, 43, 44, 47, 48, 54, 55, 56, 57, 60, 63, 8, 9, 0, 13, 14, 2, 20, 21, 26, 29, 3, 31, 34, 35, 36, 38, 39, 4, 41, 43, 44, 45, 46, 48, 49, 5, 50, 53, 54, 55, 56, 58, 59, 60, 1, 14, 21, 22, 23, 24, 25, 27, 28, 31, 32, 34, 35, 38, 42, 44, 47, 52, 54, 56, 57, 58, 59, 60, 61, 63, 7, 9, 1, 13, 14, 15, 16, 17, 18, 19, 2, 21, 22, 24, 25, 26, 27, 3, 30, 31, 32, 33, 35, 37, 38, 4, 40, 41, 42, 44, 45, 47, 49, 50, 51, 52, 53, 55, 56, 57, 58, 59, 61, 62, 63, 7, 10, 11, 14, 16, 17, 18, 19, 2, 20, 22, 23, 25, 27, 32, 33, 34, 35, 36, 37, 40, 41, 43, 46, 47, 48, 49, 50, 51, 52, 54, 57, 58, 59, 6, 61, 62, 7, 8, 9, 0, 10, 11, 13, 16, 18, 19, 21, 22, 24, 26, 27, 29, 3, 31, 32, 33, 38, 39, 41, 42, 45, 46, 5, 50, 51, 52, 53, 54, 58, 59, 6, 61, 62, 7, 9, 12, 13, 17, 19, 2, 20, 23, 24, 25, 27, 28, 3, 30, 31, 33, 34, 37, 38, 40, 43, 47, 5, 50, 51, 53, 54, 55, 58, 59, 6, 60, 63, 9, 0, 1, 10, 11, 16, 17, 18, 2, 20, 21, 23, 24, 25, 26, 27, 29, 3, 30, 35, 39, 40, 42, 43, 46, 5, 50, 52, 53, 54, 55, 56, 57, 59, 6, 62, 7, 9, 0, 11, 13, 14, 17, 2, 21, 22, 27, 28, 29, 3, 30, 32, 4, 43, 47, 5, 50, 52, 53, 54, 55, 56, 57, 58, 6, 63, 7, 9, 1, 12, 13, 16, 19, 20, 23, 28, 29, 3, 31, 33, 34, 35, 36, 37, 38, 39, 42, 43, 49, 5, 51, 52, 53, 57, 61, 62, 7, 8, 9, 0, 1, 13, 15, 17, 18, 19, 20, 21, 24, 26, 27, 28, 29, 3, 30, 31, 34, 38, 39, 4, 41, 44, 45, 47, 49, 5, 53, 54, 55, 56, 60, 8, 0, 13, 15, 16, 17, 19, 2, 20, 21, 23, 25, 3, 31, 32, 34, 37, 38, 40, 42, 48, 49, 50, 54, 55, 56, 58, 60, 63, 11, 12, 13, 14, 15, 16, 17, 19, 21, 25, 27, 29, 30, 31, 36, 37, 38, 39, 40, 42, 46, 49, 50, 51, 53, 54, 57, 59, 6, 62, 9
  };
  int dist[] = {
      43717, 65142, 61356, 29314, 834, 2887, 6569, 49447, 28158, 14405, 54606, 1922, 51093, 5802, 22413, 28299, 49980, 15885, 27037, 39291, 5182, 45157, 23797, 9086, 33263, 47345, 43878, 56908, 2557, 18129, 2937, 58342, 30609, 22945, 9559, 11302, 43717, 20643, 50223, 31900, 39628, 2483, 11370, 6770, 56353, 53897, 61448, 27903, 11691, 12003, 3933, 61637, 8714, 10959, 57704, 40054, 31876, 38004, 32228, 33884, 28359, 53394, 19356, 16950, 13657, 49547, 31574, 51496, 16352, 22995, 65418, 51035, 49447, 30484, 50826, 23739, 50881, 4839, 21595, 64692, 24065, 26593, 51912, 53100, 36393, 6023, 51303, 49569, 62466, 48300, 41987, 43373, 34516, 48232, 15224, 4687, 54150, 10177, 35805, 58452, 28475, 54082, 26547, 3933, 31428, 57726, 44118, 42974, 35040, 53207, 56142, 34916, 26344, 52230, 41643, 35771, 2636, 17000, 26059, 38237, 48744, 39965, 18574, 63836, 63248, 39623, 52485, 12807, 50653, 16323, 64385, 7186, 24451, 36488, 41132, 38788, 28952, 17627, 49980, 50107, 58028, 35117, 37191, 4479, 36394, 3407, 37565, 27997, 25971, 53184, 36371, 48822, 25050, 19100, 63610, 12956, 10348, 21311, 37204, 1740, 60445, 15502, 12254, 6029, 606, 36625, 64683, 34989, 21184, 13657, 15005, 20447, 60567, 5785, 27601, 56181, 23988, 5094, 40997, 64940, 44854, 13760, 49478, 15736, 42915, 47827, 64967, 59891, 24608, 50454, 48298, 21698, 62632, 50483, 22245, 30720, 51679, 3778, 31667, 58342, 40768, 23941, 55986, 52057, 4096, 41378, 15213, 8747, 55382, 65254, 30015, 40942, 58673, 48836, 9848, 44247, 30426, 24350, 34989, 53672, 35830, 58789, 43603, 33366, 59507, 30720, 28787, 40047, 57919, 33338, 63919, 47244, 19377, 23634, 15766, 334, 57867, 2061, 8694, 24400, 63616, 54082, 3614, 27259, 40740, 3430, 43397, 27106, 41959, 28952, 53796, 7003, 26686, 4179, 29044, 39734, 48263, 47318, 34438, 16845, 4529, 51378, 17776, 43280, 39642, 12303, 7782, 23634, 56027, 49485, 58105, 9559, 20333, 8693, 34930, 22286, 59295, 45354, 4425, 4697, 13553, 24112, 38899, 17627, 10722, 35227, 48495, 26569, 1171, 4974, 9077, 41158, 52501, 18366, 10695, 9856, 33908, 54398, 15766, 61034, 47023, 49485, 11302, 9543, 34950, 65025, 17847, 27065, 26547, 4847, 27433, 50285, 44627, 38502, 51403, 59666, 18030, 62523, 12140, 24893, 61245, 31667, 21752, 57681, 51333, 4386, 41679, 8158, 25554, 3007, 334, 46059, 60261, 58105, 24784, 23389, 44566, 44987, 47364, 50338, 53409, 30484, 9230, 52043, 44868, 49767, 269, 31428, 17605, 19735, 48777, 58444, 47803, 19945, 43440, 50107, 20933, 60870, 8641, 20565, 20529, 44920, 15005, 28208, 64318, 28136, 40768, 57867, 20333, 65142, 24784, 32237, 29218, 61368, 318, 50826, 47537, 31510, 28567, 60630, 14382, 58580, 57726, 35560, 63225, 53465, 27289, 52907, 58028, 22877, 52150, 4809, 20447, 56213, 36977, 2072, 25320, 53377, 23941, 19415, 9543, 61356, 42504, 10621, 3514, 48302, 23739, 64844, 7349, 25055, 50141, 26978, 22729, 32328, 38373, 19380, 24209, 9878, 6079, 36014, 13301, 36972, 6975, 13803, 43475, 50475, 30015, 6761, 9535, 47014, 55986, 19471, 60008, 34950, 29314, 20643, 23389, 42504, 47635, 24628, 63760, 15419, 24758, 39851, 44118, 62824, 62087, 52073, 35117, 31110, 13365, 58584, 33449, 10022, 46511, 35519, 16552, 6355, 13748, 47892, 1074, 5362, 52057, 43384, 52540, 62458, 12305, 8693, 834, 50223, 47635, 59152, 28837, 63714, 1229, 33879, 17921, 42974, 62615, 21119, 34767, 38941, 13456, 33434, 3220, 35613, 28228, 58978, 27841, 8306, 36259, 13015, 34392, 16610, 2061, 34930, 31900, 44566, 32237, 10621, 56615, 58409, 49082, 50881, 1950, 46996, 32823, 6382, 34023, 39783, 45749, 17295, 22440, 47541, 35645, 60363, 12534, 45722, 37191, 14639, 28962, 65164, 60567, 51617, 58578, 24732, 43348, 65025, 39628, 44987, 44682, 31155, 15360, 60845, 50121, 3542, 2592, 3436, 39935, 52613, 44795, 32668, 56756, 4479, 26388, 42269, 276, 57732, 16350, 31942, 12647, 24774, 64756, 2269, 4096, 16678, 48479, 15670, 8694, 22286, 2887, 2483, 47364, 29218, 3514, 59152, 56615, 44682, 60833, 62793, 18497, 33418, 33694, 6360, 23651, 35040, 12996, 37208, 2384, 10403, 13887, 36394, 35410, 51805, 57725, 5785, 11291, 35867, 31664, 21469, 23973, 33983, 31223, 41378, 12642, 1587, 15695, 59295, 6569, 11370, 50338, 61368, 48302, 28837, 58409, 60833, 4839, 60526, 57527, 10935, 8975, 51389, 63536, 49510, 39546, 20777, 3407, 39792, 32011, 42199, 44872, 23635, 11663, 52621, 29338, 47426, 32234, 44282, 42410, 24400, 45354, 17847, 6770, 53409, 318, 24628, 63714, 49082, 31155, 62793, 21595, 17342, 1590, 51014, 24816, 53810, 60454, 15052, 24633, 19870, 35521, 3100, 47654, 58239, 50810, 27601, 39192, 33277, 13533, 27543, 2506, 15213, 60613, 34718, 42179, 38515, 63616, 27065, 56353, 9230, 47537, 64844, 1950, 18497, 17342, 64692, 7032, 12994, 61614, 53207, 56980, 10426, 48533, 15595, 61127, 40051, 1415, 50819, 10035, 791, 58861, 28508, 30247, 46332, 53280, 8209, 38889, 3856, 24753, 8747, 58358, 35618, 40062, 28158, 53897, 7349, 63760, 46996, 60526, 24065, 1551, 59652, 44987, 3785, 59251, 29843, 62364, 4650, 37565, 46183, 54254, 1802, 47050, 133, 3905, 52771, 56181, 62204, 12668, 44828, 2940, 7745, 50698, 15347, 29834, 22379, 3614, 4425, 4847, 31510, 25055, 1229, 33418, 1590, 7032, 1551, 58188, 5464, 56142, 33025, 50744, 12012, 47479, 48471, 51114, 27997, 53992, 48662, 12775, 28598, 62655, 39437, 62125, 59844, 2415, 58785, 55735, 27259, 61448, 52043, 50141, 15419, 32823, 57527, 28358, 45295, 8537, 34916, 38544, 23514, 31625, 62885, 23832, 7072, 7571, 16755, 25971, 18306, 5559, 50835, 57215, 39366, 24130, 46678, 23988, 33549, 22211, 49054, 37340, 30798, 23542, 27903, 44868, 28567, 26978, 33879, 6382, 33694, 10935, 51014, 26593, 12994, 59652, 28358, 52283, 53838, 26344, 37133, 18217, 18238, 31293, 48142, 7125, 40627, 25040, 22608, 38805, 62714, 22099, 391, 43087, 63336, 1314, 19544, 36186, 34592, 55382, 16449, 40740, 4697, 14405, 22729, 34023, 15360, 51912, 61614, 45295, 52283, 53946, 50137, 57259, 30992, 57831, 45035, 23243, 60228, 43817, 6400, 43445, 32581, 5094, 49795, 23749, 10671, 44306, 33443, 65254, 60875, 4040, 13553, 54606, 11691, 60630, 24758, 39783, 60845, 6360, 24816, 53100, 44987, 36922, 52230, 33920, 4887, 51694, 20604, 34766, 6253, 32259, 12997, 27315, 35861, 33040, 40997, 37035, 61488, 44958, 56925, 20216, 35020, 3430, 27433, 49767, 14382, 32328, 50121, 8975, 3785, 53838, 53946, 41971, 48075, 41643, 49657, 1829, 23430, 570, 9110, 42222, 53184, 25942, 41523, 64615, 14170, 61640, 56457, 64940, 62258, 34420, 30246, 35225, 9505, 23635, 48219, 21922, 18919, 43397, 24112, 50285, 1922, 12003, 269, 58580, 39851, 17921, 45749, 3542, 23651, 51389, 59251, 58188, 8537, 50137, 36922, 41971, 35771, 45483, 23577, 49612, 1292, 6230, 36371, 28004, 64306, 56880, 47607, 60329, 60851, 60395, 14222, 51923, 30015, 60348, 59899, 27106, 38899, 51093, 38373, 17295, 2592, 63536, 5464, 48075, 2636, 32013, 46508, 60666, 48278, 33395, 15376, 46102, 51601, 41507, 50918, 30978, 32123, 58774, 6442, 3332, 40281, 40942, 16783, 12306, 20859, 41959, 44627, 17605, 19380, 3436, 12996, 56980, 38544, 37133, 57259, 33920, 32013, 17000, 12608, 25893, 29314, 48822, 27962, 63154, 14381, 39281, 32815, 16404, 44854, 39175, 33, 24802, 63396, 61218, 51766, 58673, 14786, 58490, 10722, 61637, 19735, 24209, 62824, 22440, 39935, 37208, 53810, 36393, 33025, 23514, 18217, 30992, 49657, 46508, 12608, 38523, 23040, 49202, 23831, 14969, 25050, 16297, 47048, 3799, 9254, 60177, 62860, 8770, 60314, 47959, 8156, 37642, 8478, 5802, 8714, 35560, 62615, 47541, 52613, 49510, 50744, 31625, 18238, 4887, 1829, 45483, 23868, 54454, 57894, 19100, 15446, 22131, 63847, 17363, 27694, 6978, 41669, 18764, 16452, 29141, 40962, 39769, 1521, 21318, 35227, 48777, 63225, 62087, 21119, 35645, 39546, 10426, 12012, 62885, 31293, 60666, 26059, 25893, 38523, 23868, 26545, 60464, 59198, 63610, 56378, 58890, 1122, 831, 28522, 32018, 41794, 18592, 42465, 1956, 13900, 16157, 40314, 24169, 53796, 48495, 10959, 9878, 52073, 2384, 60454, 6023, 48533, 23832, 23430, 23577, 48278, 23040, 54454, 19599, 12956, 58904, 42703, 32222, 10371, 12262, 13760, 60266, 23475, 21449, 50647, 17016, 36021, 41188, 34724, 12392, 7003, 26569, 38502, 58444, 53465, 6079, 34767, 60363, 44795, 15052, 51303, 15595, 48142, 570, 49612, 49202, 26545, 60071, 6050, 45592, 64905, 53274, 40860, 26151, 49478, 19816, 17633, 5587, 5885, 57917, 43009, 48836, 19128, 26686, 1171, 57704, 47803, 27289, 32668, 10403, 47479, 57831, 51694, 9110, 1292, 23831, 60464, 60071, 19788, 10348, 27789, 20795, 27668, 19251, 64589, 25844, 15736, 23475, 32148, 9848, 58347, 22430, 4974, 22413, 40054, 19945, 52907, 36014, 56756, 13887, 20777, 61127, 29843, 48471, 7072, 45035, 20604, 33395, 38237, 19788, 30687, 21311, 4530, 23315, 19763, 21198, 6732, 22289, 4410, 4998, 44247, 25206, 13358, 61589, 4179, 9077, 38941, 12534, 49569, 40051, 62364, 51114, 7571, 7125, 29314, 59198, 19599, 39411, 50043, 2392, 16076, 42915, 53151, 43298, 44019, 20065, 12059, 30426, 39844, 35671, 3818, 24316, 41158, 51403, 28299, 31876, 43440, 13301, 13456, 45722, 24633, 1415, 4650, 16755, 40627, 34766, 42222, 6230, 15376, 14969, 57894, 6050, 30687, 55778, 57386, 34472, 63666, 22678, 39861, 1420, 11772, 14741, 57100, 24350, 44962, 5233, 55247, 29044, 15885, 38004, 20933, 22877, 36972, 31110, 33434, 26388, 39792, 62466, 50819, 46183, 25942, 27962, 15446, 56378, 45592, 27789, 55778, 37204, 47617, 7636, 16621, 47827, 63455, 33666, 2212, 61034, 25377, 32699, 37186, 39734, 59666, 32228, 6975, 32011, 19870, 53992, 18306, 23243, 41523, 28004, 46102, 48744, 63154, 22131, 64905, 20795, 4530, 57386, 1740, 4207, 15034, 59571, 28102, 41535, 10555, 64967, 54238, 4753, 18730, 51016, 31399, 18030, 27037, 60870, 13803, 3220, 14639, 35521, 48300, 10035, 54254, 48662, 5559, 60228, 6253, 64615, 64306, 51601, 39965, 14381, 63847, 58890, 53274, 47617, 4207, 62883, 30568, 55514, 38651, 59161, 59891, 53060, 65207, 10138, 20122, 37720, 44081, 53672, 10095, 43072, 39104, 48263, 39291, 33884, 8641, 13365, 42269, 42199, 3100, 41987, 1802, 12775, 50835, 25040, 32259, 18574, 17363, 27668, 39411, 15034, 62883, 9847, 23364, 15990, 58560, 63992, 1989, 25879, 58880, 14003, 62427, 36700, 57311, 52501, 5182, 28359, 43475, 58584, 35410, 791, 28598, 22608, 12997, 14170, 56880, 41507, 1122, 19251, 23315, 34472, 59571, 9847, 6642, 63742, 10793, 40809, 48905, 9217, 35830, 60915, 47318, 18366, 62523, 45157, 20565, 52150, 35613, 44872, 58861, 47050, 57215, 43817, 27315, 50918, 27694, 831, 58904, 64589, 19763, 7636, 30568, 23364, 40791, 10679, 19296, 5376, 7932, 36842, 34438, 12140, 23797, 53394, 20529, 33449, 28228, 28962, 276, 28508, 133, 62655, 39366, 35861, 63836, 39281, 16297, 6978, 28522, 42703, 63666, 60445, 40791, 58957, 41268, 22216, 2203, 25114, 58804, 58789, 42302, 24893, 19356, 44920, 50475, 23635, 47654, 43373, 30247, 3905, 39437, 24130, 38805, 6400, 63248, 47048, 32018, 32222, 40860, 21198, 50043, 22678, 15502, 28102, 55514, 15990, 6642, 30771, 7967, 21469, 747, 33258, 56754, 11171, 16500, 43603, 48429, 61245, 9086, 16950, 30015, 10022, 57732, 51805, 11663, 58239, 52771, 62714, 43445, 61640, 47607, 30978, 39623, 32815, 41669, 41794, 10371, 26151, 25844, 2392, 39861, 12254, 41535, 38651, 58560, 63742, 30771, 7118, 2676, 11058, 36168, 33366, 47431, 10695, 33263, 4809, 6761, 46511, 65164, 16350, 57725, 52621, 50810, 46332, 46678, 22099, 32581, 33040, 56457, 60329, 16404, 18592, 12262, 6732, 16076, 6029, 16621, 10555, 59161, 58957, 24608, 19093, 30066, 10629, 59507, 4113, 39096, 20989, 2833, 16845, 9856, 47345, 49547, 35519, 58978, 11291, 34516, 53280, 391, 39175, 18764, 42465, 60266, 1420, 63455, 53060, 63992, 10679, 41268, 7967, 7118, 50454, 36924, 16168, 40650, 17491, 41210, 46926, 16621, 28787, 7196, 39158, 4529, 21752, 43878, 31574, 56213, 9535, 16552, 31942, 35867, 39192, 48232, 43087, 37035, 60851, 32123, 52485, 33, 3799, 23475, 19816, 65207, 1989, 10793, 21469, 2676, 57267, 19704, 53500, 14305, 2516, 29067, 33908, 57681, 56908, 6355, 27841, 15224, 8209, 62204, 61488, 58774, 12807, 9254, 21449, 17633, 23475, 53151, 11772, 606, 54238, 25879, 40809, 19296, 22216, 11058, 19093, 48298, 36924, 53831, 38924, 34990, 9304, 40325, 24221, 21376, 51496, 8306, 12668, 62125, 33549, 63336, 49795, 62258, 60395, 60177, 16452, 50647, 5587, 43298, 10138, 48905, 747, 53831, 55471, 43698, 31092, 62319, 7703, 10177, 41899, 42896, 51378, 51333, 16352, 13748, 36259, 51617, 12647, 31664, 29338, 33277, 4687, 44828, 59844, 1314, 23749, 44958, 34420, 50653, 24802, 62860, 29141, 1956, 5885, 22289, 44019, 36625, 33666, 4753, 20122, 9217, 5376, 33258, 30066, 16168, 57267, 38924, 55471, 37045, 25665, 23718, 4273, 25089, 21604, 33308, 54195, 17776, 28208, 36977, 13015, 24774, 21469, 47426, 13533, 54150, 38889, 2415, 22211, 10671, 30246, 40962, 13900, 17016, 57917, 32148, 4410, 2212, 18730, 58880, 2203, 56754, 36168, 10629, 40650, 19704, 34990, 37045, 49789, 18850, 15345, 40047, 28673, 43556, 43280, 54398, 4386, 2557, 64318, 2072, 47892, 64756, 32234, 27543, 2940, 58785, 19544, 56925, 35225, 6442, 16323, 8770, 39769, 16157, 20065, 14741, 51016, 37720, 7932, 25114, 21698, 17491, 53500, 9304, 43698, 25665, 40084, 40654, 57919, 39569, 56887, 39642, 41679, 47014, 1074, 23973, 2506, 10177, 3856, 49054, 36186, 44306, 9505, 14222, 64385, 63396, 60314, 40314, 36021, 4998, 12059, 61034, 14003, 11171, 62632, 41210, 14305, 31092, 23718, 49789, 52463, 63008, 33338, 55365, 21178, 8158, 18129, 22995, 28136, 25320, 2269, 33983, 44282, 35805, 24753, 7745, 37340, 34592, 33443, 20216, 23635, 3332, 7186, 61218, 43009, 57100, 25377, 44081, 62427, 58804, 50483, 46926, 40325, 62319, 4273, 18850, 40084, 52463, 25135, 63919, 59649, 12303, 25554, 2937, 53377, 5362, 34392, 31223, 58452, 50698, 55735, 48219, 51923, 40281, 24451, 51766, 1521, 64683, 36700, 16500, 22245, 16621, 24221, 7703, 25089, 15345, 40654, 63008, 25135, 47244, 17200, 7782, 3007, 65418, 19471, 43384, 16678, 60613, 58358, 30798, 60348, 16783, 36488, 47959, 24169, 41188, 19128, 58347, 25206, 39844, 44962, 10095, 57311, 4113, 51679, 2516, 21376, 10177, 55365, 4412, 39943, 56027, 61034, 46059, 30609, 51035, 52540, 58578, 12642, 42410, 34718, 35618, 15347, 16449, 35020, 21922, 59899, 12306, 41132, 14786, 8156, 34724, 35671, 5233, 21184, 31399, 60915, 36842, 48429, 39096, 3778, 41899, 21604, 28673, 39569, 4412, 47023, 22945, 62458, 24732, 48479, 1587, 42179, 28475, 40062, 29834, 23542, 60875, 38788, 37642, 21318, 12392, 13358, 3818, 32699, 43072, 47431, 20989, 7196, 33308, 43556, 56887, 59649, 39943, 3188, 19415, 60008, 12305, 16610, 43348, 15670, 15695, 38515, 22379, 4040, 18919, 20859, 58490, 8478, 22430, 61589, 24316, 55247, 37186, 39104, 42302, 2833, 39158, 29067, 42896, 54195, 21178, 17200, 19377, 3188, 60261
  };
  int edge[] = {
      0, 36, 72, 103, 138, 169, 199, 236, 271, 302, 334, 369, 401, 434, 468, 496, 528, 560, 598, 632, 668, 703, 739, 768, 801, 840, 870, 902, 939, 976, 1006, 1038, 1072, 1104, 1139, 1172, 1204, 1232, 1265, 1293, 1327, 1360, 1391, 1431, 1463, 1492, 1519, 1549, 1585, 1621, 1658, 1691, 1722, 1754, 1782, 1826, 1865, 1901, 1934, 1971, 2001, 2032, 2065, 2093
  };

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

#define mx 500000

int main(){
  int mx1=mx/n;
  clock_t t0=clock();
  for(int h=0;h<10;h++){
    int lng=0;
    for(int n1=0;n1<n;n1++){
      for(int k1=0;k1<mx1;k1++){
        int start=n1;
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
        cur = n-1;
        while(cur!=start){
          lng+=wght[cur];
          cur = prev[cur];
        }
      }
    }
    printf("done %d\n",lng);
    double t2=(double)(clock()-t0)/CLOCKS_PER_SEC;
    double t3= t2/(h+1);
    double t4= t3*1000000/(mx);
    printf("time %f %f %f\n",t3,t2,t4);
  }
  return 0;
}

/*

C:\mads>dijkstra3.exe
done 1965233412
time 4.515000 4.515000 9.030000
done 1965233412
time 4.609000 9.218000 9.218000
done 1965233412
time 4.552000 13.656000 9.104000
done 1965233412
time 4.531000 18.124000 9.062000
done 1965233412
time 4.524800 22.624000 9.049600
done 1965233412
time 4.536333 27.218000 9.072667
done 1965233412
time 4.540143 31.781000 9.080286
done 1965233412
time 4.542875 36.343000 9.085750
done 1965233412
time 4.539889 40.859000 9.079778
done 1965233412
time 4.542100 45.421000 9.084200

C:\mads>pause
Press any key to continue . . .
*/