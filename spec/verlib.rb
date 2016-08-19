class Ver
  def initialize(version)
    @version = version.split('.').map(&:to_i)
  end
end

puts Ver.new('1.2.3').inspect
