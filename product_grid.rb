class ProductGrid
  MAX = 20
  def initialize
    @grid = read_in_grid("11.txt")
    @max_prod = 1
  end

  def read_in_grid(fname)
    grid = Array.new(20) { Array.new(20, 0) }
    File.open(fname, "r") do |f|
      0.upto(MAX-1).each do |r|
        f.readline.split(" ").each_with_index do |num, col|
          grid[r][col] = num.to_i
        end
      end
    end
    grid
  end

  def get_largest_diag_dl_products(grid)
    max_prod = 1
    max_sq = 1
    cur = 0
    0.upto(MAX-4).each do |r|
      4.upto(MAX-1).each do |c|
        cur = grid[r][c] * grid[r+1][c-1] * grid[r+2][c-2] * grid[r+3][c-3]
        # p "DIAG-DOWN: #{r},#{c} * #{r+1},#{c+1} * #{r+2},#{c+2} * #{r+3},#{c+3} => #{cur}"
        if cur > max_prod
          max_prod = cur
          max_sq = "#{r},#{c}"
        end
      end
    end
    puts "highest diag-dl product was #{max_prod} on square: #{max_sq}"
    max_prod
  end


  def get_largest_diag_dr_products(grid)
    max_prod = 1
    max_sq = 1
    cur = 0
    0.upto(MAX-4).each do |r|
      0.upto(MAX-4).each do |c|
        cur = grid[r][c] * grid[r+1][c+1] * grid[r+2][c+2] * grid[r+3][c+3]
        # p "DIAG-DR: #{r},#{c} * #{r+1},#{c+1} * #{r+2},#{c+2} * #{r+3},#{c+3} => #{cur}"
        if cur > max_prod
          max_prod = cur
          max_sq = "#{r},#{c}"
        end
      end
    end
    puts "highest diag-dr product was #{max_prod} on square: #{max_sq}"
    max_prod
  end
  
  def get_largest_across_prods(grid)
    max_prod = 1
    max_sq = 1
    cur = 0
    0.upto(MAX-1).each do |r|
      0.upto(MAX-4).each do |c|
        cur = grid[r][c] * grid[r][c+1] * grid[r][c+2] * grid[r][c+3]
        # p "ACROSS: #{r},#{c} * #{r},#{c+1} * #{r},#{c+2} * #{r},#{c+3} => #{cur}"
        if cur > max_prod
          max_prod = cur
          max_sq = "#{r},#{c}"
        end
      end
    end
    puts "highest across product was #{max_prod} on square: #{max_sq}"
    max_prod
  end

  def get_largest_down_prods(grid)
    max_prod = 1
    max_sq = 1
    cur = 0
    0.upto(MAX-4).each do |r|
      0.upto(MAX-1).each do |c|
        cur = grid[r][c] * grid[r+1][c] * grid[r+2][c] * grid[r+3][c]
        # p "DOWN: #{r},#{c} * #{r+1},#{c} * #{r+2},#{c} * #{r+3},#{c} => #{cur}"
        if cur > max_prod
          max_prod = cur
          max_sq = "#{r},#{c}"
        end
      end
    end
    puts "highest down product was #{max_prod} on square: #{max_sq}"
    max_prod
  end
  
  def find_largest_prods()
    max = []
    max[0] = get_largest_diag_dl_products(@grid)
    max[3] = get_largest_diag_dr_products(@grid)
    max[1] = get_largest_across_prods(@grid)
    max[2] = get_largest_down_prods(@grid)
    puts "found max #{max.max}"
  end
end
