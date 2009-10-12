load 'deploy' if respond_to?(:namespace) # cap2

set :application, 'dotfiles'
set :scm, :git
set :deploy_via, :copy
set :copy_exclude, ['.git']
set :repository, 'git@duien.com:dotfiles'
set :deploy_to, 'dotfiles'

set :use_sudo, false
set :transfer_via, :scp
set(:custom_server) { printf "Server to push to: "; STDOUT.flush; STDIN.readline.chomp }
role :app, custom_server

task :push do
  strategy.deploy!
end 
