FROM opensuse:42.1

RUN zypper -n in ruby glibc-i18ndata glibc-locale
RUN localedef -i en_US -f UTF-8 en_US.UTF-8