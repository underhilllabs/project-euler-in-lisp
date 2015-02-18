# my Project Euler ruby methods!!

def solve_20
  (1..100).reduce(:*).to_s.split("").reduce {|acc, num| acc.to_i + num.to_i}
end

# sum multiples of 3 or 5 upto n                                                                          
def sum_mult_3_or_5(n)                                                                                    
 (1..n).select{|n| n % 3 == 0 or n % 5 == 0 }.reduce(:+)                                                  
end        

require 'mathn'
# I had to work REALLY HARD for this answer!!
def largest_prime_factor(n)
   Prime.prime_division(n).last.first
end
