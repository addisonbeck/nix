terraform {
  required_providers {
    digitalocean = {
      source = "digitalocean/digitalocean"
      version = "2.36.0"
    }
  }
}

# An API key for authenticating with Digital Ocean.
# This can be created from the "API" section of the Digital Ocean dashboard.
variable "DIGITAL_OCEAN_API_KEY" {
  type = string
  sensitive = true
}

# The hostname for the droplet that will be created
variable "DROPLET_HOST_NAME" {
  type = string
}

# An optional domain to assign to the droplet
variable "DROPLET_DOMAIN_NAME" {
  type = string
  default = ""
}

# The region to upload images to & create droplets on
variable "DROPLET_REGION" {
  type = string
  default = "nyc3"
}

# An array of SSH public keys to add to the droplet. 
# At least one is required if you want to be able to ssh to the droplet. 
variable "DROPLET_ALLOWED_PUBLIC_SSH_KEYS" {
  type = list(number)
  default = []
}

# The SKU for the droplet that will be created
variable "DROPLET_SKU" {
  type = string
  default = "s-1vcpu-1gb"
}

# From here we'll start creating resources. In English the subsequent steps
# achieve the following in chronological order:
# 1. Authenticated with Digital Ocean
# 2. Pulling a NixOs image from the internet and uploading it as a custom
#    image to Digital Ocean
# 3. Creating a droplet
# 4. Attaching a domain, optionally based on provided input

provider "digitalocean" {
  token = var.DIGITAL_OCEAN_API_KEY
}

resource "digitalocean_custom_image" "nixos_unstable" {
  name    = "nixos_unstable"
  url     = "https://github.com/hraban/nixos-images/releases/download/latest/nixos-digitalocean-x86_64-linux.qcow.gz"
  regions = [var.DROPLET_REGION]
}

resource "digitalocean_droplet" "development" {
  image    = digitalocean_custom_image.nixos_unstable.id
  name     = var.DROPLET_HOST_NAME
  region   = var.DROPLET_REGION
  size     = var.DROPLET_SKU
  ssh_keys = var.DROPLET_ALLOWED_PUBLIC_SSH_KEYS
}

resource "digitalocean_domain" "development_domain" {
  count      = var.DROPLET_DOMAIN_NAME != "" ? 1 : 0
  name       = var.DROPLET_DOMAIN_NAME
  ip_address = digitalocean_droplet.development.ipv4_address
}

output "droplet_ip" {
  value = digitalocean_droplet.development.ipv4_address
}
