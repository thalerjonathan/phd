# installing haskell and running the STM case-studies on AMAZON# inspired by:
#https://gist.github.com/letsspeak/5650158
#http://logicaltypes.blogspot.com/2015/08/recipe-getting-haskell-running-on-aws.html
#https://stackoverflow.com/questions/32089252/install-gcc-on-aws-ec2-without-using-yum


# NOTE: extend the harddisk space to 32Gbyte because 8GByte is to small
# extend volume through console and then tell the instance OS to extend the volume

# extending the volume in the OS
df -h
sudo growpart /dev/xvda 1
sudo resize2fs /dev/xvda1

# install gcc and other development tools
sudo yum groupinstall "Development Tools"

# install dependencies
sudo yum -y install gmp-devel
# install libGLU / GL required by gloss
# sudo yum install libGLU 
sudo yum install freeglut-devel.x86_64

# download haskell-platform into separate folder
mkdir haskell
cd haskell
# download
wget https://haskell.org/platform/download/8.4.3/haskell-platform-8.4.3-unknown-posix--full-x86_64.tar.gz
# unzip
tar -xzf haskell-platform-8.4.3-unknown-posix--full-x86_64.tar.gz
# install
sudo sh ./install-haskell-platform.sh

# clone phd repo 
git clone https://github.com/thalerjonathan/phd.git 
# navigate to STM paper code
cd phd/public/stmabs/code/SugarScape/SugarScapeSTMTArray/
# build 
stack build
# execute
sh ./run.sh

# cabal --http-transport=plain-http update
