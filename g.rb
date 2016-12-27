$settings = {
  whitespace: {
    files: [
      'home/.emacs.d/config.org',
      'home/.emacs.d/config.org',
      'home/.emacs.d/init.el',
      'home/.zshrc.org',
      'doc.org'
    ]
  }
}

module ZHelpers
  extend RSpec::Matchers::DSL
  matcher :have_no_whitespace do
    match do |line|
      line.sub(/[ \t]+$/, '') == line
    end
    chain
    failure_message do |actual|
      "expected that #{actual.inspect} would have no whitespace"
    end
  end
end
