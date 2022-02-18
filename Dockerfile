# based on https://dev.to/cloudx/testing-our-package-build-in-the-docker-world-34p0

FROM archlinux AS build

# binutils is for `strip` even though we're not using it
RUN pacman -Sy --noconfirm sudo fakeroot binutils gcc pkg-config glib2 make

WORKDIR /build

COPY PKGBUILD .
COPY podripper.sh LICENSE \
    rssgen/.stack-work/install/x86_64-linux-*/*/*/bin/rssgen-exe \
    podripper/
# multiple steps are necessary to copy every individual directory
ADD conf podripper/conf
ADD rssgen/conf podripper/rssgen/conf
ADD systemd podripper/systemd

# setup a regular build user because makepkg refuses to run as root
RUN useradd --system builduser \
    && passwd -d builduser \
    && (printf 'builduser ALL=(ALL) ALL\n' | tee -a /etc/sudoers) \
    && chown -R builduser /build \
    # archiving all files because makepkg doesn't like globbed files in `source`
    && tar cf podripper.tar podripper \
    && sudo -u builduser bash -c 'makepkg --noconfirm --nodeps'

CMD ["bash"]

# this allows to copy the built package to the host
# https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs
FROM scratch AS export
COPY --from=build /build/podripper*pkg* /
