# my Project Euler ruby methods!!

def solve_20
  (1..100).reduce(:*).to_s.split("").reduce {|acc, num| acc.to_i + num.to_i}
end
