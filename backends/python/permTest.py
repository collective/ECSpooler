def test(list1, list2):
    """
    Returns True if list1 is a permutation of list2
    """
    result = (len(list1) == len(list2))
    
    if result is True:
        for item in list1:
            if not item in list2:
                result = False
                break 
    
    return result