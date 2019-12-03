quicklisp_url="https://beta.quicklisp.org/quicklisp.lisp"
quicklisp_install_script="(quicklisp-quickstart:install)"

all: install-quicklisp

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
