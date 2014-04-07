dotfiles_dir="$HOME/gitlab/dotfiles"
# zsh

ln -sv $dotfiles_dir/zsh/.zshrc $HOME/.zshrc
ln -sv $dotfiles_dir/zsh/.zshrc.aliases $HOME/.zshrc.aliases
ln -sv $dotfiles_dir/zsh/.fizsh $HOME/.fizsh
ln -sv $dotfiles_dir/zsh/.zsh $HOME/.zsh

# Vim
echo "======================== VIM! ================================="
ln -sv $dotfiles_dir/Vim/.vimrc $HOME/.vimrc
ln -sv $dotfiles_dir/Vim $HOME/.vim

# tmux
ln -sv $dotfiles_dir/.tmux.conf $HOME/.tmux.conf
