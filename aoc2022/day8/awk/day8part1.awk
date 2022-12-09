BEGIN { FS = "" }
{
    # Create a 2d-array of all the entries
    if (max_nf < NF)
	max_nf = NF
    max_nr = NR
    for (x = 1; x <= NF; x++)
    {
	arr[NR,x] = $x
	# Also initialise a same-sized array of 0's
	spottable[NR,x] = 0
    }
}
END {
    # For each row left->right
    for (y = 1; y <= max_nr; y++)
    {
	lowest = arr[y,1]
	spottable[y,1] = 1
	for (x = 2; x < max_nf; x++) {
	    if (lowest < arr[y,x]) { spottable[y,x] = 1 }
	    # spottable[y,x] = lowest < arr[y,x] ? 1 : 0
	    lowest = lowest < arr[y,x] ? arr[y,x] : lowest
	}
    }
    # For each row right->left
    for (y = 1; y <= max_nr; y++)
    {
	lowest = arr[y,max_nf]
	spottable[y,max_nf] = 1
	for (x = max_nf-1; x > 1; x--) {
	    if (lowest < arr[y,x]) { spottable[y,x] = 1 }
	    lowest = lowest < arr[y,x] ? arr[y,x] : lowest
	}
    }

    # For each column up->down
    for (x = 1; x <= max_nf; x++)
    {
	lowest = arr[1,x]
	spottable[1,x] = 1
	for (y = 2; y < max_nr; y++) {
	    if (lowest < arr[y,x]) { spottable[y,x] = 1 }
	    lowest = lowest < arr[y,x] ? arr[y,x] : lowest
	}
    }

    # For each column down->up
    for (x = 1; x <= max_nf; x++)
    {
	lowest = arr[max_nr,x]
	spottable[max_nr,x] = 1
	for (y = max_nr-1; y > 1; y--) {
	    if (lowest < arr[y,x]) { spottable[y,x] = 1 }
	    lowest = lowest < arr[y,x] ? arr[y,x] : lowest
	}
    }

    # Finally, sum all the entries
    sum = 0
    for (x = 1; x <= max_nf; x++) {
	for (y = 1; y <= max_nr; y++)
	{
	    sum = sum + spottable[x,y]
	}
    }
    printf("Spottable: %s\n", sum)
}
