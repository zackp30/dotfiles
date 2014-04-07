require 'sshkit/dsl'

on %w{zackp30@cadoth.co pi@172.23.32.6} do
  within "~/gitlab/dotfiles" do
    execute "git", "pull", "origin", "master"
  end
end

