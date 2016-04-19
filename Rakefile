pngs = FileList['**/*.png']

task :optipng => pngs.ext('.opti-png')

rule '.opti-png' => '.png' do |t|
  sh "optipng #{t.source} -o7 -out #{t.name}"
end
