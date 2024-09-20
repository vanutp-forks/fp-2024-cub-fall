# HW02
## Soft Deadline: 23:59 24.09.2024
## Hard Deadline: 23:59 26.09.2024

1. (1 points) Implement `multByIndex :: [Int] -> [Int]` that multiplies each element of the list by its index. 
   * `multByIndex [1,2,3] == [1 * 0, 2 * 1, 3 * 2]`

2. (1 points) Implement `powerByIndex :: [Int] -> [Int]` that raises each element in a list to the power of its index.
   * `powerByIndex [1,2,3] == [1 ^ 0, 2 ^ 1, 3 ^ 2]` 

3. (1 points) Implement `productOfDifference :: [Int] -> Int` that computes the product of difference between neighboring elements 
   * `productOfDifference [4,2,1] == [4 - 2, 2 - 1]`
   * Throw `error` if the list is too short. 
   * You can use `@` to introduce a name to a pattern matched expression.  

4. (1 points) Implement `isSorted :: [Int] -> Bool` that checks if the list is sorted. 
   * `isSorted [0..10] == True`
   * `isSorted [0, 1, 412, 10] == False`

5. (1 points) Implement `countElement :: Int -> [Int] -> Int` that counts how many times its first argument occurs in the list. 
   * `countElement 42 [0..100] == 1` 
   * `countElement 42 [0..10] == 0` 
   * `countElement 42 (replicate 13 42) == 13`

6. (1 points) Implement `dotProduct :: [Int] -> [Int] -> Int` that computes the dot-product of two lists.
   * `dotProduct [1,2,3] [4,5,6] == [1*4, 2*5, 3*6]` 
   * If the lists differ in length, the result should have the shortest length. 

7. (1 points) Implement `applyAll :: [a -> b] -> a -> [b]` that applies every function in the list to the second argument. 
   * `applyAll [(+1), (*2), (3-)] 42 == [42+1, 42*2, 3-42]` 

8. (1 points) Implement `interleave :: [a] -> [a] -> [a]` that interleaves the elements of the two lists. 
   * `interleave [1,2,3] [4,5,6] == [1,4,2,5,3,6]` 
   * If the lists differ in length, the result should have twice the shortest length.

## Notes 

* Make a fork of this repository and checkout the branch `HW02`.
* Write your code in the `Main.hs` file: replace `undefined` with your definitions.
* Make sure that your tests pass, i.e. running `main` (or `./Main`) only outputs `Done`.
* If you need more tests, add them, but don't delete the ones which are already in the file. 
* When finished, open a pull request into the main repo. Make sure to put your name in the title of the PR.  