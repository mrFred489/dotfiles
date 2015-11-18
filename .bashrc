# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

primary_monitor() {
# Credit: http://ubuntuforums.org/showthread.php?t=1309247
echo "Enter the primary display from the following:"			# prompt for the display
xrandr --prop | grep "[^dis]connected" | cut --delimiter=" " -f1	# query connected monitors
 
read choice								# read the users's choice of monitor
 
xrandr --output $choice --primary					# set the primary monitor

}

live() {
qual="source"
chatting="no"
quals=( "audio" "low" "medium" "high" "source" )

if [ "$#" -gt 1 ]; then
	if [ $2 = "chat" ]; then
		google-chrome http://www.twitch.tv/"$1"/chat?popout=
		chatting = "yes"
		echo chatting1
		if [ "$#" -gt 2 ]; then
			for name in ${quals[@]}
                	do
                	        
                	        if [ $3 == $name ]; then
                        	        qual="$3"
                        	        echo "it is a quality"
                        	fi
                	done
		fi
	else
		for name in ${quals[@]}
		do
			
			if [ $2 == $name ]; then
				qual="$2"
				echo "it is a quality"
				if [ $3 = "chat" ]; then
                			open -a safari http://www.twitch.tv/"$1"/chat?popout=
					echo chatting2
                			chatting = "yes"
                                else
                                        echo http://www.twitch.tv/"$1"/chat?popout=
				fi
			fi
		done
	fi
	
fi
if [ $chatting != "yes" ]; then
	echo http://www.twitch.tv/"$1"/chat?popout=
fi
if [ "$#" -gt 0 ]; then
	livestreamer twitch.tv/"$1" $qual
else
	echo "i need a stream to watch"
fi
echo $qual

}

change_wallpaper(){
    
    bash /home/frederik/Scripts/wallpaper.sh>>/home/frederik/Scripts/test.log
}

cdl(){
	if [[ -z $1 ]]; then
	    cd
	else
		cd "$1"
	fi
	ls -ABgGch --color=auto $2
}

subr() {
    google-chrome https://reddit.com/r/"$1"
}

export LANG="en_DK.UTF-8"

alias skyp="PULSE_PROP='filter.want=echo-cancel' skype"
alias settings="gnome-control-center"
alias calculator="gnome-calculator"
alias system-monitor="gnome-system-monitor"
alias filebrowser="nautilus --no-desktop"
alias disk-usage-analyzer="baobab"
alias sushu="sudo shutdown -h now"
alias openMinecraft="java -jar /home/frederik/Applications/Minecraft.jar"
alias openPyCharm="bash /home/frederik/Applications/pycharm/bin/pycharm.sh"
alias pdfviewer="evince"
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

primary_monitor() {
# Credit: http://ubuntuforums.org/showthread.php?t=1309247
echo "Enter the primary display from the following:"			# prompt for the display
xrandr --prop | grep "[^dis]connected" | cut --delimiter=" " -f1	# query connected monitors
 
read choice								# read the users's choice of monitor
 
xrandr --output $choice --primary					# set the primary monitor

}

live() {
qual="source"
chatting="no"
quals=( "audio" "low" "medium" "high" "source" )

if [ "$#" -gt 1 ]; then
	if [ $2 = "chat" ]; then
		google-chrome http://www.twitch.tv/"$1"/chat?popout=
		chatting = "yes"
		echo chatting1
		if [ "$#" -gt 2 ]; then
			for name in ${quals[@]}
                	do
                	        
                	        if [ $3 == $name ]; then
                        	        qual="$3"
                        	        echo "it is a quality"
                        	fi
                	done
		fi
	else
		for name in ${quals[@]}
		do
			
			if [ $2 == $name ]; then
				qual="$2"
				echo "it is a quality"
				if [ $3 = "chat" ]; then
                			open -a safari http://www.twitch.tv/"$1"/chat?popout=
					echo chatting2
                			chatting = "yes"
                                else
                                        echo http://www.twitch.tv/"$1"/chat?popout=
				fi
			fi
		done
	fi
	
fi
if [ $chatting != "yes" ]; then
	echo http://www.twitch.tv/"$1"/chat?popout=
fi
if [ "$#" -gt 0 ]; then
	livestreamer twitch.tv/"$1" $qual
else
	echo "i need a stream to watch"
fi
echo $qual

}

change_wallpaper(){
    
    bash /home/frederik/Scripts/wallpaper.sh>>/home/frederik/Scripts/test.log
}

cdl(){
	if [[ -z $1 ]]; then
	    cd
	else
		cd "$1"
	fi
	ls -ABgGch --color=auto $2
}

subr() {
    google-chrome https://reddit.com/r/"$1"
}

export LANG="en_DK.UTF-8"

alias skyp="PULSE_PROP='filter.want=echo-cancel' skype"
alias settings="gnome-control-center"
alias calculator="gnome-calculator"
alias system-monitor="gnome-system-monitor"
alias filebrowser="nautilus --no-desktop"
alias disk-usage-analyzer="baobab"
alias sushu="sudo shutdown -h now"
alias openMinecraft="java -jar /home/frederik/Applications/Minecraft.jar"
alias openPyCharm="bash /home/frederik/Applications/pycharm/bin/pycharm.sh"
alias pdfviewer="evince"

