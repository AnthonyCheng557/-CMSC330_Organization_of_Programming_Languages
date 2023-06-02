def fib(n)
    fibArray = Array.new
    for i in 0..n-1 
        if i == 0 
            fibArray[i] = 0
        elsif i == 1
            fibArray[i] = 1
        else
        fibArray[i] = fibArray[i-1] + fibArray[i-2]
        end
    end
    return fibArray
end

def isPalindrome(n)
    array = n.to_s.chars
    first = 0
    last = array.length()
    length = (last/2).to_i
    for i in 1..length
        if array[first] != array[last-1]
            return false
        end
        first = first + 1
        last = last - 1
    end
    return true;
end

def nthmax(n, a)
    if n > a.length()
        return nil
    end
    array = a.sort.reverse
    temp = array[n]
    return temp
end

def freq(s)
    if s.length == 0
        return ""
    end
    array = s.chars.sort
    current = array[0]
    count = 0
    max = 0
    temp = Array.new
    for i in 0..array.length()-1
        if current != array[i]
            current = array[i]
            count = 1
        elsif current == array[i]
            count = count + 1
        end
        if count > max
            max = count
                temp[0] = array[i]
            end
        end
        return temp[0].to_s
    end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length
        return nil
    end
    temp = Hash.new
    for i in 0..arr1.length()-1
        temp[arr1[i]] = arr2[i]
    end
    return temp
end

def hashToArray(hash)
    return hash.to_a
end

def maxProcChain(init, procs)
    value = init;
    for i in 0..procs.length()-1
        if value < procs[i].call(value)
            value = procs[i].call(value)
        else
            value = procs[i].call(procs[i-1].call(value))
        end
    end
    return value
end
