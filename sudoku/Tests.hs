import Logic
import Debug.Trace

--------------------------- make a test sudoku ----------------------------------------

c00 = Cell {value = 0, coords = (0,0), block = 00}
c01 = Cell {value = 0, coords = (0,1), block = 00}
c02 = Cell {value = 0, coords = (0,2), block = 00}
c03 = Cell {value = 1, coords = (0,3), block = 01}
c04 = Cell {value = 0, coords = (0,4), block = 01}
c05 = Cell {value = 5, coords = (0,5), block = 01}
c06 = Cell {value = 0, coords = (0,6), block = 02}
c07 = Cell {value = 6, coords = (0,7), block = 02}
c08 = Cell {value = 8, coords = (0,8), block = 02}
row0 = [c00,c01,c02,c03,c04,c05,c06,c07,c08]

c10 = Cell {value = 0, coords = (1,0), block = 00}
c11 = Cell {value = 0, coords = (1,1), block = 00}
c12 = Cell {value = 0, coords = (1,2), block = 00}
c13 = Cell {value = 0, coords = (1,3), block = 01}
c14 = Cell {value = 0, coords = (1,4), block = 01}
c15 = Cell {value = 0, coords = (1,5), block = 01}
c16 = Cell {value = 7, coords = (1,6), block = 02}
c17 = Cell {value = 0, coords = (1,7), block = 02}
c18 = Cell {value = 1, coords = (1,8), block = 02}
row1 = [c10,c11,c12,c13,c14,c15,c16,c17,c18]

c20 = Cell {value = 9, coords = (2,0), block = 00}
c21 = Cell {value = 0, coords = (2,1), block = 00}
c22 = Cell {value = 1, coords = (2,2), block = 00}
c23 = Cell {value = 0, coords = (2,3), block = 01}
c24 = Cell {value = 0, coords = (2,4), block = 01}
c25 = Cell {value = 0, coords = (2,5), block = 01}
c26 = Cell {value = 0, coords = (2,6), block = 02}
c27 = Cell {value = 3, coords = (2,7), block = 02}
c28 = Cell {value = 0, coords = (2,8), block = 02}
row2 = [c20,c21,c22,c23,c24,c25,c26,c27,c28]

c30 = Cell {value = 0, coords = (3,0), block = 10}
c31 = Cell {value = 0, coords = (3,1), block = 10}
c32 = Cell {value = 7, coords = (3,2), block = 10}
c33 = Cell {value = 0, coords = (3,3), block = 11}
c34 = Cell {value = 2, coords = (3,4), block = 11}
c35 = Cell {value = 6, coords = (3,5), block = 11}
c36 = Cell {value = 0, coords = (3,6), block = 12}
c37 = Cell {value = 0, coords = (3,7), block = 12}
c38 = Cell {value = 0, coords = (3,8), block = 12}
row3 = [c30,c31,c12,c33,c34,c35,c36,c37,c38]

c40 = Cell {value = 5, coords = (4,0), block = 10}
c41 = Cell {value = 0, coords = (4,1), block = 10}
c42 = Cell {value = 0, coords = (4,2), block = 10}
c43 = Cell {value = 0, coords = (4,3), block = 11}
c44 = Cell {value = 0, coords = (4,4), block = 11}
c45 = Cell {value = 0, coords = (4,5), block = 11}
c46 = Cell {value = 0, coords = (4,6), block = 12}
c47 = Cell {value = 0, coords = (4,7), block = 12}
c48 = Cell {value = 3, coords = (4,8), block = 12}
row4 = [c40,c41,c42,c43,c44,c45,c46,c47,c48]

c50 = Cell {value = 0, coords = (5,0), block = 10}
c51 = Cell {value = 0, coords = (5,1), block = 10}
c52 = Cell {value = 0, coords = (5,2), block = 10}
c53 = Cell {value = 8, coords = (5,3), block = 11}
c54 = Cell {value = 7, coords = (5,4), block = 11}
c55 = Cell {value = 0, coords = (5,5), block = 11}
c56 = Cell {value = 4, coords = (5,6), block = 12}
c57 = Cell {value = 0, coords = (5,7), block = 12}
c58 = Cell {value = 0, coords = (5,8), block = 12}
row5 = [c50,c51,c52,c53,c54,c55,c56,c57,c58]

c60 = Cell {value = 0, coords = (6,0), block = 20}
c61 = Cell {value = 3, coords = (6,1), block = 20}
c62 = Cell {value = 0, coords = (6,2), block = 20}
c63 = Cell {value = 0, coords = (6,3), block = 21}
c64 = Cell {value = 0, coords = (6,4), block = 21}
c65 = Cell {value = 0, coords = (6,5), block = 21}
c66 = Cell {value = 8, coords = (6,6), block = 22}
c67 = Cell {value = 0, coords = (6,7), block = 22}
c68 = Cell {value = 5, coords = (6,8), block = 22}
row6 = [c60,c61,c62,c63,c64,c65,c66,c67,c68]

c70 = Cell {value = 1, coords = (7,0), block = 20}
c71 = Cell {value = 0, coords = (7,1), block = 20}
c72 = Cell {value = 5, coords = (7,2), block = 20}
c73 = Cell {value = 0, coords = (7,3), block = 21}
c74 = Cell {value = 0, coords = (7,4), block = 21}
c75 = Cell {value = 0, coords = (7,5), block = 21}
c76 = Cell {value = 0, coords = (7,6), block = 22}
c77 = Cell {value = 0, coords = (7,7), block = 22}
c78 = Cell {value = 0, coords = (7,8), block = 22}
row7 = [c70,c71,c72,c73,c74,c75,c76,c77,c78]

c80 = Cell {value = 0, coords = (8,0), block = 20}
c81 = Cell {value = 0, coords = (8,1), block = 20}
c82 = Cell {value = 0, coords = (8,2), block = 20}
c83 = Cell {value = 0, coords = (8,3), block = 21}
c84 = Cell {value = 0, coords = (8,4), block = 21}
c85 = Cell {value = 0, coords = (8,5), block = 21}
c86 = Cell {value = 7, coords = (8,6), block = 22}
c87 = Cell {value = 0, coords = (8,7), block = 22}
c88 = Cell {value = 1, coords = (8,8), block = 22}
row8 = [c80,c81,c82,c83,c84,c85,c86,c87,c88]

testSudoku = [row0,row1,row2,row3,row4,row5,row6,row7,row8]





















t00 = Cell {value = 0, coords = (0,0), block = 00}
t01 = Cell {value = 3, coords = (0,1), block = 00}
t02 = Cell {value = 5, coords = (0,2), block = 00}
t03 = Cell {value = 2, coords = (0,3), block = 01}
t04 = Cell {value = 6, coords = (0,4), block = 01}
t05 = Cell {value = 9, coords = (0,5), block = 01}
t06 = Cell {value = 7, coords = (0,6), block = 02}
t07 = Cell {value = 8, coords = (0,7), block = 02}
t08 = Cell {value = 1, coords = (0,8), block = 02}
r0 = [t00,t01,t02,t03,t04,t05,t06,t07,t08]

t10 = Cell {value = 6, coords = (1,0), block = 00}
t11 = Cell {value = 8, coords = (1,1), block = 00}
t12 = Cell {value = 2, coords = (1,2), block = 00}
t13 = Cell {value = 5, coords = (1,3), block = 01}
t14 = Cell {value = 7, coords = (1,4), block = 01}
t15 = Cell {value = 1, coords = (1,5), block = 01}
t16 = Cell {value = 4, coords = (1,6), block = 02}
t17 = Cell {value = 9, coords = (1,7), block = 02}
t18 = Cell {value = 3, coords = (1,8), block = 02}
r1 = [t10,t11,t12,t13,t14,t15,t16,t17,t18]

t20 = Cell {value = 1, coords = (2,0), block = 00}
t21 = Cell {value = 9, coords = (2,1), block = 00}
t22 = Cell {value = 7, coords = (2,2), block = 00}
t23 = Cell {value = 8, coords = (2,3), block = 01}
t24 = Cell {value = 3, coords = (2,4), block = 01}
t25 = Cell {value = 4, coords = (2,5), block = 01}
t26 = Cell {value = 5, coords = (2,6), block = 02}
t27 = Cell {value = 6, coords = (2,7), block = 02}
t28 = Cell {value = 2, coords = (2,8), block = 02}
r2 = [t20,t21,t22,t23,t24,t25,t26,t27,t28]

t30 = Cell {value = 8, coords = (3,0), block = 10}
t31 = Cell {value = 2, coords = (3,1), block = 10}
t32 = Cell {value = 6, coords = (3,2), block = 10}
t33 = Cell {value = 1, coords = (3,3), block = 11}
t34 = Cell {value = 9, coords = (3,4), block = 11}
t35 = Cell {value = 5, coords = (3,5), block = 11}
t36 = Cell {value = 3, coords = (3,6), block = 12}
t37 = Cell {value = 4, coords = (3,7), block = 12}
t38 = Cell {value = 7, coords = (3,8), block = 12}
r3 = [t30,t31,t12,t33,t34,t35,t36,t37,t38]

t40 = Cell {value = 3, coords = (4,0), block = 10}
t41 = Cell {value = 7, coords = (4,1), block = 10}
t42 = Cell {value = 4, coords = (4,2), block = 10}
t43 = Cell {value = 6, coords = (4,3), block = 11}
t44 = Cell {value = 8, coords = (4,4), block = 11}
t45 = Cell {value = 2, coords = (4,5), block = 11}
t46 = Cell {value = 9, coords = (4,6), block = 12}
t47 = Cell {value = 1, coords = (4,7), block = 12}
t48 = Cell {value = 5, coords = (4,8), block = 12}
r4 = [t40,t41,t42,t43,t44,t45,t46,t47,t48]

t50 = Cell {value = 9, coords = (5,0), block = 10}
t51 = Cell {value = 5, coords = (5,1), block = 10}
t52 = Cell {value = 1, coords = (5,2), block = 10}
t53 = Cell {value = 7, coords = (5,3), block = 11}
t54 = Cell {value = 4, coords = (5,4), block = 11}
t55 = Cell {value = 3, coords = (5,5), block = 11}
t56 = Cell {value = 6, coords = (5,6), block = 12}
t57 = Cell {value = 2, coords = (5,7), block = 12}
t58 = Cell {value = 8, coords = (5,8), block = 12}
r5 = [t50,t51,t52,t53,t54,t55,t56,t57,t58]

t60 = Cell {value = 5, coords = (6,0), block = 20}
t61 = Cell {value = 1, coords = (6,1), block = 20}
t62 = Cell {value = 9, coords = (6,2), block = 20}
t63 = Cell {value = 3, coords = (6,3), block = 21}
t64 = Cell {value = 2, coords = (6,4), block = 21}
t65 = Cell {value = 6, coords = (6,5), block = 21}
t66 = Cell {value = 8, coords = (6,6), block = 22}
t67 = Cell {value = 7, coords = (6,7), block = 22}
t68 = Cell {value = 4, coords = (6,8), block = 22}
r6 = [t60,t61,t62,t63,t64,t65,t66,t67,t68]

t70 = Cell {value = 2, coords = (7,0), block = 20}
t71 = Cell {value = 4, coords = (7,1), block = 20}
t72 = Cell {value = 8, coords = (7,2), block = 20}
t73 = Cell {value = 9, coords = (7,3), block = 21}
t74 = Cell {value = 5, coords = (7,4), block = 21}
t75 = Cell {value = 7, coords = (7,5), block = 21}
t76 = Cell {value = 1, coords = (7,6), block = 22}
t77 = Cell {value = 3, coords = (7,7), block = 22}
t78 = Cell {value = 6, coords = (7,8), block = 22}
r7 = [t70,t71,t72,t73,t74,t75,t76,t77,t78]

t80 = Cell {value = 7, coords = (8,0), block = 20}
t81 = Cell {value = 6, coords = (8,1), block = 20}
t82 = Cell {value = 3, coords = (8,2), block = 20}
t83 = Cell {value = 4, coords = (8,3), block = 21}
t84 = Cell {value = 1, coords = (8,4), block = 21}
t85 = Cell {value = 8, coords = (8,5), block = 21}
t86 = Cell {value = 2, coords = (8,6), block = 22}
t87 = Cell {value = 5, coords = (8,7), block = 22}
t88 = Cell {value = 9, coords = (8,8), block = 22}
r8 = [t80,t81,t82,t83,t84,t85,t86,t87,t88]

testSudoku2 = [r0,r1,r2,r3,r4,r5,r6,r7,r8]
