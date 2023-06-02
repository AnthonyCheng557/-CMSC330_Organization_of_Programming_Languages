/**
Returns the sum 1 + 2 + ... + n
If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1;
    } else {
        let mut x = n;
        let mut a = n;
        while x > 0{
        x = x - 1;
        a = a + x;
        }
        return a;
    }
}

/**
Returns the number of elements in the list that 
are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut count = 0;
    let mut index = 0;
    while index < ls.len() {
        if (ls[index] >= s) && (ls[index] <= e) {
            count = count + 1;
        }
        index = index + 1;
    }
    return count;
}

/**
Returns true if target is a subset of set, false otherwise

Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    if set.len() == 0 { 
        return false;
    }
    if target.len() == 0 {
        return true;
    }
    let mut t_i = 0;
    let mut s_i = 0;
    let mut flag = false;
    while s_i < set.len() {
        while t_i < target.len() {
            if set[s_i] == target[t_i] {
                flag = true;
            } 
            t_i = t_i + 1;
        }
        if flag != true {
            return false;
        }
        s_i = s_i + 1;
    } 
    return true;
}


/**
Returns the mean of elements in ls. If the list is empty, return None
It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let mut total = 0.0;
    let mut i = 0;
    if ls.len() == 0 {
        return None;
    }
    while i < ls.len() {
        total = total + ls[i];
        i = i + 1;
    }
    return Some(total / (ls.len() as f64))
}

/**
Converts a binary number to decimal, where each bit is stored in order in the array
    
Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut temp = ls.to_owned();
    temp.reverse();
    let mut total = 0;
    let mut i = 0;
    while i < temp.len() {
        if temp[i] == 1 {
            total = total + 2_i32.pow(i as u32);
        }
        i = i + 1;
    }
    return total;
}

/**
Decomposes an integer into its prime factors and returns them in a vector
You can assume factorize will never be passed anything less than 2

Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn possible_primes(n: u32) -> Vec<u32> {
    let mut all_prime = Vec::new();
    let mut num = 2;
    let mut is_prime = true;
    while num <= n {
        let mut curr = num - 1;
        while curr > 1 {
            if num % curr == 0 {
                is_prime = false;
            }
            curr = curr -1;
        }
        if is_prime {
            all_prime.push(num);
        }
        num = num + 1;
        is_prime = true;
    }
    return all_prime;
}

pub fn factorize(n: u32) -> Vec<u32> {
    let mut y = Vec::new();
    let x = possible_primes(n);
    let mut i = 0;
    let mut curr = n;
    while i < x.len() {
        if curr % x[i] == 0 {
            y.push(x[i]);
            curr /= x[i];
        } else {
            i += 1;
        }
    }
    return y;
}

/** 
Takes all of the elements of the given slice and creates a new vector.
The new vector takes all the elements of the original and rotates them, 
so the first becomes the last, the second becomes first, and so on.

EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    if lst.len() == 0 {
        return Vec::new();
    }
    let mut i =1;
    let mut new_vector = Vec::new();
    
    while i < lst.len() {
        new_vector.push(lst[i]);
        i += 1;
    }
    new_vector.push(lst[0]);
    return new_vector;
}

/**
Returns true if target is a subtring of s, false otherwise
You should not use the contains function of the string library
in your implementation
    
Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if s == "" {
        return true;
    }
    let mut i = 0;
    let mut curr = "";
    while i <= s.len() {
        let mut j = i;
        while j <= s.len() {
            curr = &s[i..j];
            if curr == target {
                return true;
            }
            j += 1;
        }
        i += 1;
    }
    return false;
}
/**
Takes a string and returns the first longest substring of
consecutive equal characters

EX: longest_sequence of "ababbba" is Some("bbb")
EX: longest_sequence of "aaabbb" is Some("aaa")
EX: longest_sequence of "xyz" is Some("x")
EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s == "" {
        return None;
    }

    let mut tuples = Vec::new();
    let mut i = 0;
    
    let mut curr = "";
    let mut count = 0;
    let mut start = 0;
    
    while i < s.len() {
       if curr != &s[i..i+1] {
            tuples.push((curr, count, start));
            curr = &s[i..i+1];
            count = 1;
            start = i;
       } else {
           count += 1;
       }
       i+=1;
        
    }
    tuples.push((curr, count, start));
    
    let mut max = 0;
    let mut j = 0;
    let mut the_max_tuple = ("", 0, 0); //temp
    while j < tuples.len() {
        if tuples[j].1 > max {
            the_max_tuple = tuples[j];
            max = tuples[j].1;
        }
        j+=1;
    }
    
    return Some(&s[(the_max_tuple.2)..(the_max_tuple.1 + the_max_tuple.2)]);
    
}
