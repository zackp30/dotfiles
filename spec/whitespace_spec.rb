require './g.rb'

describe 'File' do
  $settings[:whitespace][:files].each do |g|
    next if
    describe g do
      line = 0
      IO.foreach g do |l|
        line += 1
        describe "Line #{line}" do
          it "should have no trailing whitespace" do
            expect(l.sub(/[ \t]+$/, '')).to eql(l)
          end
        end
      end
    end
  end
end
