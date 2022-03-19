FROM archlinux

# binutils is for `strip` even though we're not using it
# glibc needs to be updated because gcc fails otherwise
RUN pacman -Sy --noconfirm sudo fakeroot binutils gcc glibc pkg-config make

WORKDIR /build

ADD streamripper streamripper/

## setup a regular build user because makepkg refuses to run as root
RUN useradd --system builduser \
    && passwd -d builduser \
    && (printf 'builduser ALL=(ALL) ALL\n' | tee -a /etc/sudoers) \
    && chown -R builduser /build \
    # archiving all files because makepkg doesn't like globbed files in `source`
    #&& tar cf podripper.tar podripper \
    && sudo -u builduser bash -c 'cd streamripper && makepkg --noconfirm --syncdeps --install'
