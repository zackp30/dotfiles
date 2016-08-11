describe 'File' do
  include ZHelpers
  $settings[:whitespace][:files].each do |g|
    describe g do
      line = 0
      IO.foreach g do |l|
        line += 1
        describe "Line #{line}" do
          it 'should have no trailing whitespace' do
            expect(l).to have_no_whitespace
          end
        end
      end
    end
  end
end
