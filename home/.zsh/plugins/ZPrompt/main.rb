require 'time'
class String
  def fg
    #"%K{#{color}}#{text}%k"
    puts "#{self}"
  end
  def bg(color)
    #"%F{#{color}}#{self}%f"
    "%F{#{color}}#{self}%f"
  end
  def bgfg(one, two)
    "%F{#{one}}%K{#{two}}#{self}%f%k"
  end
end
module ZPrompt
  class Funcs
    def gettime
      Time.now.strftime('%T')
    end
  end
  class Init
    f = Funcs.new
    #puts "#{fg.colorb('red', 'blue', '%*')}"
    puts ("#{f.gettime}".bgfg('red', 'blue') + '%~'.bgfg('green', 'black') + "@#{`hostname`}".chomp.bgfg('red', 'blue') + ' ').chomp
  end
end
