# syntax=docker/dockerfile:1
#
# rocker/geospatial provides R, sf, GDAL and PROJ. Versioned rocker images pin
# CRAN to a dated Posit Package Manager snapshot, so any packages installed
# below resolve to fixed versions.
FROM rocker/geospatial:4.4.2

# Optional: trust an extra CA certificate (e.g. for a TLS-inspecting corporate
# proxy). Pass it at build time with:
#   docker build --secret id=extra_ca,src=path/to/ca.crt .
RUN --mount=type=secret,id=extra_ca,required=false \
    if [ -s /run/secrets/extra_ca ]; then \
      cp /run/secrets/extra_ca /usr/local/share/ca-certificates/extra-ca.crt && \
      update-ca-certificates; \
    fi

# rclone, for uploading packaged requests to Google Drive.
ARG RCLONE_VERSION=1.68.2
ARG TARGETARCH=amd64
RUN curl -fsSL -o /tmp/rclone.zip \
      "https://github.com/rclone/rclone/releases/download/v${RCLONE_VERSION}/rclone-v${RCLONE_VERSION}-linux-${TARGETARCH}.zip" && \
    unzip -q /tmp/rclone.zip -d /tmp && \
    install -m 0755 "/tmp/rclone-v${RCLONE_VERSION}-linux-${TARGETARCH}/rclone" /usr/local/bin/rclone && \
    rm -rf /tmp/rclone*

# R packages not already in the base image go here (install2.r --error --skipinstalled ...);
# they resolve from the pinned snapshot.

WORKDIR /app
COPY . /app
RUN R CMD INSTALL --no-docs --no-multiarch /app

EXPOSE 3838
CMD ["Rscript", "-e", "ciarequests::run_app(host = '0.0.0.0', port = 3838)"]
