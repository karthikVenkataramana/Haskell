{- -------------------------------------------------------------------------- -
 - -------------------------------------------------------------------------- -
 -                                                                            -
 - Copyright (c) 2016, Karthik Venkataramana Pemmaraju                        -
 -                                                                            -
 - Redistribution and use in source and binary forms, with or without         -
 - modification, are permitted provided that the following conditions         -
 - are met:                                                                   -
 -                                                                            -
 -     1. Redistributions of source code must retain the above copyright      -
 -     notice, this list of conditions and the following disclaimer.          -
 -                                                                            -
 -     2. Redistributions in binary form must reproduce the above copyright   -
 -     notice, this list of conditions and the following disclaimer in the    -
 -     documentation and/or other materials provided with the distribution.   -
 -                                                                            -
 -     3. Neither the name of the copyright holder nor the names of its       -
 -     contributors may be used to endorse or promote products derived from   -
 -     this software without specific prior written permission.               -
 -                                                                            -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS    -
 - IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED      -
 - TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A            -
 - PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT         -
 - HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,     -
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   -
 - TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR     -
 - PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF     -
 - LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING       -
 - NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         -
 - SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               -
 -                                                                            -
 - -------------------------------------------------------------------------- -
 - -------------------------------------------------------------------------- -}
 
 {-
Authors: 1) Karthik Venkataramana Pemmaraju

Date: 01/29/2017 - 01/30/2017

Description: This Haskell program generates all possible combinations of a binary tree.
Also, counts the number of leaves, internal nodes  and establishes a relation between them.

Compilation : :load allTrees.hs

Execution: 1) generateTrees "node-name" number-of-nodes (Ex: generateTrees "Karthik" 3)
           2) number_of_internal_nodes t1 (t1 is a sample tree defined below)
           3) number_of_leafs t1
           4) catalan_numbers (Would verify length of our generated Trees with first 10 Catalan numbers).

Output: SEE AT THE BOTTOM.
-}

{- 
   Sample Tree (t1) for Checking 
                Department
                   |     
            __________________
            |                 |
           CSE                IT
            |
        __________
        |         |
       CS        CE
-}
t1 = Branch "Department" (Branch "CSE" (Branch "CS" EMPTY EMPTY) (Branch "CE" EMPTY EMPTY)) (Branch "IT" EMPTY EMPTY)
    

-- A Binary Tree data constructor definition.
data BTree a = EMPTY | Branch a (BTree a) (BTree a) deriving (Eq,Show,Read)

-- A leaf can be thought of an empty branch.
leaf x = Branch x EMPTY EMPTY 


--Counts total number of nodes in the Tree.
size::BTree String -> Int
size (Branch _ EMPTY EMPTY) = 1
size (Branch _ left right) = 1+ (size left)+ (size right) 

-- Number of internal nodes in Binary Tree
number_of_internal_nodes::BTree String ->Int
number_of_internal_nodes (Branch _ left right) 
 | left /= EMPTY && right /= EMPTY = 1 + (number_of_internal_nodes left) + (number_of_internal_nodes right)
 | otherwise = 0 + (number_of_internal_nodes left) + (number_of_internal_nodes right)
number_of_internal_nodes (EMPTY)=0

-- Number of leafs in a Binary Tree
number_of_leafs::BTree String->Int
number_of_leafs EMPTY = 0
number_of_leafs (Branch _ EMPTY EMPTY)= 1
number_of_leafs (Branch _ left right) = 0+ (number_of_leafs left) + (number_of_leafs right)

-- Generates all possible binary trees for given size n (See Catalan numbers)
generateTrees::String->Int->[BTree String]
generateTrees _ 0 = []
generateTrees x 1 = [leaf x]
generateTrees x n |n>=0 = leftmostTree ++ combinedTree ++ rightmostTree where 
                          leftmostTree  = [Branch x t EMPTY | t<- generateTrees x (n-1)]
                          rightmostTree = [Branch x EMPTY t| t<- generateTrees x (n-1)]
                          combinedTree  =  concat [[Branch x left right | right <- generateTrees x (n-1-inc),left <- generateTrees x inc ] | inc <- [0..(n-1)]]

auto = mapM_  print (map (generateTrees "K") [1..3])     
{-
 Proof of Observation (Applies only for Full Binary Trees i.e Nodes with either zero or two children)

 Claim     : There exist N-1 internal nodes for N leaves
 Base case :
 			 For size=1 : Number of leaves = 1 (k)
  					      Number of internal nodes = 0 (k-1)

 Hypothesis: Assuming that for a given Binary tree with K leaves there exist K-1 internal nodes
 			 i.e for n(k), n = k and k = k-1
 Induction :  for n (k+1), n = k+1 
 				=> (k-1) +1 
 				=> n(k) +1 
 Hence the relation exists for all  Full Binary Trees of size n where n is odd and n>0
-} 

-- Catalan number verification. 
catalan_numbers = [length (generateTrees "Test" n) | n <- [1..10]] --First 10 Catalan numbers.

{-

Sample Output:

Prelude> :l allTrees.hs
[1 of 1] Compiling Main             ( Group14-Assignment1.hs, interpreted )
Ok, modules loaded: Main.
*Main> generateTrees "PL" 3
[Branch "PL" (Branch "PL" (Branch "PL" EMPTY EMPTY) EMPTY) EMPTY,Branch "PL" (Branch "PL" EMPTY (Branch "PL" EMPTY EMPTY)) EMPTY,Branch "PL" (Branch "PL" EMPTY EMPTY) (Branch "PL" EMPTY EMPTY),Branch "PL" EMPTY (Branch "PL" (Branch "PL" EMPTY EMPTY) EMPTY),Branch "PL" EMPTY (Branch "PL" EMPTY (Branch "PL" EMPTY EMPTY))]
*Main> catalan_numbers
[1,2,5,14,42,132,429,1430,4862,16796]

-}