# -*- python -*-
optipng = Builder(action = 'optipng -o7 $SOURCE -out $TARGET',
                  suffix = '.opti.png',
                  src_suffix = '.png')
env = Environment(BUILDERS = {'Optipng': optipng})

for item in Glob("screens/*.png"):
    if item not in Glob("screens/*.opti.png"):
        env.Optipng(item)
