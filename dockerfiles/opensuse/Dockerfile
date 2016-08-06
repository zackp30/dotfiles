FROM opensuse:42.1

RUN zypper -n in ruby ruby-devel glibc-i18ndata glibc-locale
RUN zypper -n in -t pattern devel_C_C++
RUN zypper -n in cmake
RUN localedef -i en_US -f UTF-8 en_US.UTF-8