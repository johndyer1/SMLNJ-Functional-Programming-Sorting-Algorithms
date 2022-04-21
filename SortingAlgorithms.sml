(*Created by John Dyer*)

(*Insertion Sort*)
    (*Moves given integer x towards position of list that is less than next integer*)
    fun insertionComp x (y::z) =
        if x < y
            then x::y::z
        else
            y::(insertionComp x z)
        |
        insertionComp x [] = [x];
    (*Recursively iterates through list once for every position*)
    fun insertionSort (x::y) = insertionComp x (insertionSort y)
        |
        insertionSort [] = [];
        


(*Bubble Sort*)
    (*Moves largest integer towards tail of list*)
    fun bubbleComp (x::y::z) =                                  
        if x > y
            then y::bubbleComp(x::z)
        else
            x::bubbleComp(y::z)
        |
        bubbleComp [x] = [x]
        |
        bubbleComp [] = [];
    (*Recursively iterates through list once for every position*)
    fun bubbleSort (x::y) = bubbleComp(x::bubbleSort(y))
        |
        bubbleSort [] = [];
        
        
        
(*Miscellaneous*)
    (*Finds size of list*)
    fun size (x::y) = 1 + size (y)
        |
        size [] = 0;
     
        
        
(*Tests*)

val l = [6,45,64,4,3,44,123,3452345,5];
size l;
bubbleSort l;
insertionSort l;
