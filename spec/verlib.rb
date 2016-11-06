class Ver
  def t_semver(version)
    @version = version
  end
  def initialize(type, version)
    case type
    when "semver"
      t_semver version
    else
      throw "Invalid error type"
    end
  end
end

puts Ver.new('semver', '1.2.3').inspect
