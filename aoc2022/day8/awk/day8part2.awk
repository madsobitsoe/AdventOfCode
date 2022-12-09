BEGIN { FS = "" }
{
    # Create a 2d-array of all the entries
    if (max_nf < NF)
	max_nf = NF
    max_nr = NR
    for (x = 1; x <= NF; x++)
    {
	arr[NR,x] = $x
    }
}
END {
    # For each tree
    for (y = 1; y <= max_nr; y++)
    {
	for (x = 1; x <= max_nf; x++) {
	    
	    # Looking up
	    up_coord=y-1
	    up_dist=0
	    while (up_coord >= 1)	    
	    {
		up_dist++
		if (arr[up_coord,x] >= arr[y,x]) { break; }
		up_coord--
	    }
	    
	    scenic[y,x] = up_dist
	    # looking down
	    down_coord=y+1
	    down_dist=0
	    # Looking down
	    while (down_coord <= max_nr)	    
	    {
		down_dist++
		if (arr[down_coord,x] >= arr[y,x]) { break; }		
		down_coord++
	    }
	    
	    scenic[y,x] = scenic[y,x] * down_dist
	    # Looking left
	    left_coord=x-1
	    left_dist=0
	    while (left_coord >= 1)	    
	    {
		left_dist++
		if (arr[y,left_coord] >= arr[y,x]) { break; }		
		left_coord--
	    }
	    scenic[y,x] = scenic[y,x] * left_dist
	    # Looking right
	    right_coord=x+1
	    right_dist=0
	    while (right_coord <= max_nf)
	    {
		right_dist++
		if (arr[y,right_coord] >= arr[y,x]) { break; }				
		right_coord++
	    }
	    scenic[y,x] = scenic[y,x] * right_dist
	    printf("y: %s, x: %s, scenic %s * %s * %s * %s = %s\n", y, x, up_dist, left_dist, down_dist, right_dist, scenic[y,x])
	}
    }

    # Finally, find the largest entry
    max = 0
    for (x = 1; x <= max_nf; x++) {
	for (y = 1; y <= max_nr; y++)
	{
	    max = scenic[y,x] > max ? scenic[y,x] : max

	}
    }
    printf("Best score: %s\n", max)
}
