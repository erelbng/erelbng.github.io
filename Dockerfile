FROM ruby:3.3-slim

# Install dependencies for Jekyll, Emacs, and htmlproofer
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        git \
        emacs-nox \
        curl \
        libffi-dev \
        libxml2-dev \
        libxslt-dev \
        zlib1g-dev \
        nodejs \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /site

# Cache bundle installs
COPY Gemfile Gemfile.lock ./
RUN gem install bundler && bundle install

# Copy everything
COPY . .

# Default command: build the site
CMD ["bundle", "exec", "jekyll", "serve", "--host", "0.0.0.0", "--watch"]
