export TERM="xterm-256color"
tmux new-session -d -s tutorial
tmux split-window -t tutorial:1 -v
tmux rename-window main
tmux send-keys -t tutorial:1.1 "vim src/clj/tutorial/core.clj" "Enter"
tmux send-keys -t tutorial:1.2 "lein repl" "Enter"
tmux new-window -t tutorial:2
tmux select-window -t tutorial:2
tmux rename-window shell
tmux select-window -t tutorial:1
tmux attach -t tutorial

