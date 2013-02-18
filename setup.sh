#!/bin/bash

# setup a chroot for clojurebot on an ec2 instance

outside() {

ubuntu=`lsb_release -a|grep Codename:|awk '{print $2}'`
location="/var/chroot/"

apt-get install dchroot debootstrap tmux

if [ ! -e $HOME/.tmux.conf ]; then
cat >> $HOME/.tmux.conf <<EOF
set -g prefix C-z
unbind C-b
bind C-z send-prefix
EOF
fi

if [ ! -e $location ]; then

cat >> /etc/schroot/schroot.conf <<EOF
[$ubuntu]
description=clojurebot
location=$location
priority=3
users=$USER
groups=sbuild
root-groups=root
EOF

debootstrap --variant=buildd --arch i386 $ubuntu $location http://us-west-1.ec2.archive.ubuntu.com/ubuntu/

mount --bind /proc $location"proc"

fi

cp /etc/apt/sources.list $location"etc/apt/sources.list"

cp "$0" $location

sudo chroot $location sh /setup.sh inside

}

inside() {

apt-get update 
apt-get install openjdk-7-jre wget
wget http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
ln -sf jetty-runner-8.1.9.v20130131.jar jetty-runner.jar

}

$1

exit 0

