(*Created by John Dyer*)
(*A collection of different sorting algorithms*)


(*Miscellaneous*)
    (*Finds size of list*)
    fun size (x::y) = 1 + size (y)
        |
        size [] = 0;
    
    (*Finds greatest int of list*)
    fun greatestInt (x::y::z) = 
        if x > y
            then greatestInt (x::z)
        else
            greatestInt (y::z)
        |
        greatestInt [x] = x;
        
    (*Finds smallest int of list*)
    fun smallestInt (x::y::z) =
        if x < y
            then smallestInt (x::z)
        else 
            smallestInt (y::z)
        |
        smallestInt [x] = x;
        
    (*Tests to see if the list is sorted and returns a bool*)
    fun isSorted (x::y::z) =
        if x <= y
            then isSorted (y::z)
        else if x > y
            then false
        else
            true
        |
        isSorted (x::y) = true;
        
    (*Converts an int to a string plus a comma*)
    fun intString (x) = Int.toString x ^ ", ";
    (*Converts a given list to a string*)
    fun listString (x::y) = intString(x)^listString(y)
        |
        listString (nil) = "";
    (*Prints list*)
    fun printList (x::y) = print (listString (x::y));
    
        
        
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
        


(*Quick Sort*)
    fun leftPartition (x::y::z) =
        if x >= y
            then y::leftPartition(x::z)
        else
            leftPartition(x::z)
        |
        leftPartition (x) = nil;
        
    fun rightPartition (x::y::z) =
        if x < y
            then y::rightPartition(x::z)
        else
            rightPartition(x::z)
        |
        rightPartition (x) = nil;
        
    fun quickSort (x::y::z) = quickSort((leftPartition(x::y::z))) @ [x] @ quickSort(rightPartition(x::y::z))
        |
        quickSort (x) = x;



(*Selection Sort*)
    (*Finds smallest integer of list that is not in same position as last*)
    (*a=current int pos, b=last int pos, c=full list size, d=current list size, e=last int pos duplicate*)
    (*w=current smallest, q=last int*)
    fun selectionComp (a, b, e, w, q, x::y) = 
        if a > e andalso x = q
            then (x, a)
        else if x < w andalso a <> b andalso x > q 
            then selectionComp ((a+1), a, e, x, q, (y))
        else
            selectionComp ((a+1), b, e, w, q, (y))
        |
        selectionComp (a, b, e, w, q, []) = (w, b);
    (*Recursively calls the selectionComp to create a list*)
    fun selectionCall (a, b, c, d, e, w, q, x::y) = 
        if c <> d
            then 
                let 
                    val (n, r) = (selectionComp(a, b, e, w, q, x::y))
                in 
                    n::(selectionCall(0, r, c, (d+1), r, (greatestInt(x::y)+1), n, x::y))
                end
        else
            nil;
    (*Gives correct parameters for selectionCall function and takes a given list*)
    fun selectionSort (x::y) = selectionCall(0, 1, size(x::y), 0, 0, (x+1), (smallestInt(x::y)-1), (x::y))
        |
        selectionSort [] = [];



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



(*Merge Sort*)
    (*Takes 2 sorted or single lists and merges them*)
    fun merge (a::b, x::y) =
        if a <= x andalso b <> nil andalso y <> nil
            then a::merge (b, x::y)
        else if a <= x andalso b <> nil andalso y = nil
            then a::merge (b, x::y)
        else if a <= x andalso b = nil andalso y <> nil
            then a::merge (nil, x::y)
        else if a <= x andalso b = nil andalso y = nil
            then a::x::merge(nil, nil)
        else if a >= x andalso b <> nil andalso y <> nil
            then x::merge (a::b, y)
        else if a >= x andalso b <> nil andalso y = nil
            then x::merge (a::b, nil)
        else if a >= x andalso b = nil andalso y <> nil
            then x::merge (a::b, y)
        else 
            x::a::merge(nil, nil)
        |
        merge (a::b, nil) = a::merge(b, nil)
        |
        merge (nil, x::y) = x::merge(nil, y)
        |
        merge (nil, nil) = [];
    (*Takes list and deconstructs it into individual components*)
    fun deconstruct (x::y) = [x]::deconstruct (y)
        |
        deconstruct ([]) = [];
    (*Recursively merges the deconstructed list*)
    fun reconstruct (x::y::z) = merge (x, y)::reconstruct (z)
        |
        reconstruct (x::y) = [x]
        |
        reconstruct ([]) = nil;
    (*Sorts list with*)
    fun mergeSorter (x::y) = 
        if size (x::y) > 1
            then mergeSorter (reconstruct (x::y))
        else
            x;
    (*Deconstructs list to give to the mergeSorter*)
    fun mergeSort (x::y) = mergeSorter (deconstruct (x::y));



(*Tests*)
    (*Prints bubble sorted list to console if functional and an error message otherwise*)
    fun bubbleSortTest (x::y) =
        if isSorted (bubbleSort (x::y))
            then printList (bubbleSort (x::y))
        else 
            print ("*bubbleSort did not return a sorted list* ");
    (*Prints insertion sorted list to console if functional and an error message otherwise*)
    fun insertionSortTest (x::y) =
        if isSorted (insertionSort (x::y))
            then printList (insertionSort (x::y))
        else 
            print ("*insertionSort did not return a sorted list* ");
    (*Prints insertion sorted list to console if functional and an error message otherwise*)
    fun quickSortTest (x::y) =
        if isSorted (quickSort (x::y))
            then printList (quickSort (x::y))
        else 
            print ("*quickSort did not return a sorted list* ");
    (*Prints selection sorted list to console if functional and an error message otherwise*)
    fun selectionSortTest (x::y) =
        if isSorted (selectionSort (x::y))
            then printList (selectionSort (x::y))
        else 
            print ("*selectionSort did not return a sorted list* ");
    (*Prints merge sorted list to console if functional and an error message otherwise*)
    fun mergeSortTest (x::y) =
        if isSorted (mergeSort (x::y))
            then printList (mergeSort (x::y))
        else 
            print ("*mergeSort did not return a sorted list* ");
            
   
    val l = [22,45,1,6,~4,4,4,3,~3,29999999,~3452,6,6,444,~200000,51,1,~21,2,~122,344,78,44,55,~11,~11];
    val m = [9999, ~9, 6, ~1000, ~1111, 65, 65, 400, 3000];
    bubbleSortTest l;
    insertionSortTest l;
    quickSortTest l;
    selectionSortTest l;
    mergeSortTest l;
