{
    part1 += $1 > previous ? 1 : 0; previous = $1
    nums[NR]=$1
} 
    
END {
    print part1-1
}

END {
    previous = 0
    for (i = 1; i <= NR-3; i++){
        sum=nums[i] + nums[i+1] + nums[i+2]
        part2 += sum > previous ? 1 : 0
        previous=sum
    }; 
    
    print part2
}