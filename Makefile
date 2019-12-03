#========================================
#CONFIGURATION/SHORTHAND 
#========================================
quicklisp_url="https://beta.quicklisp.org/quicklisp.lisp"
quicklisp_install_script="(quicklisp-quickstart:install)"

all: install-quicklisp create-user install-code install-systemd-config

#========================================
#INSTALLATION
#========================================

install-dependencies: install-quicklisp
	@echo "Done";

# Pull the latest quicklisp dist and install it. ToDo: Maybe verify, but then I'd need to keep on top of the hash or key
install-quicklisp: install-sbcl
	@echo "Installing quicklisp. "
	if [ ! -d ~/quicklisp ] ; then \
		wget -q -O/tmp/quicklisp.lisp $(quicklisp_url) && \
		sbcl --load /tmp/quicklisp.lisp --eval $(quicklisp_install_script); \
	fi;

# Detect if sbcl is installed. If it is, do nothing. Otherwise, detect
# if we're on a Red Hat based or Debian based distro, and use the
# appropriate package manager.
install-sbcl:
	which sbcl || { \
		which dnf && dnf install sbcl || true; \
		which apt-get && apt-get install sbcl -y || true; \
	} || true;

# We require the user+group 'image-comparator-daemon' exists, but we don't need a homedir or a shell.
create-user:
	useradd -M image-comparator-daemon -s /usr/sbin/nologin || true;

# Create a directory for our code:
install-code:
	mkdir -p /usr/share/image-comparator-daemon;
	cp -rv src/* /usr/share/image-comparator-daemon;

install-systemd-config:
	cp conf/image-comparator-daemon.service /etc/systemd/system/;
# Double check the permissions:
	chmod 644 /etc/systemd/system/image-comparator-daemon.service;

	systemctl daemon-reload;
	systemctl start image-comparator-daemon;
	systemctl enable image-comparator-daemon;

#========================================
#UNINSTALLATION
#========================================

uninstall: uninstall-code delete-user uninstall-systemd-config
	@echo "Done"

delete-user:
	userdel image-comparator-daemon || true;

uninstall-code:
	rm -rf /usr/share/image-comparator-daemon;

uninstall-systemd-config:
	systemctl disable image-comparator-daemon;
	systemctl stop image-comparator-daemon;
	rm -f /etc/systemd/system/image-comparator-daemon.service;
	systemctl daemon-reload;
