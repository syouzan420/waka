# Shipnix recommended settings
# IMPORTANT: These settings are here for ship-nix to function properly on your server
# Modify with care

{ config, pkgs, modulesPath, lib, ... }:
{
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
    settings = {
      trusted-users = [ "root" "ship" "nix-ssh" ];
    };
  };

  programs.git.enable = true;
  programs.git.config = {
    advice.detachedHead = false;
  };

  services.openssh = {
    enable = true;
    # ship-nix uses SSH keys to gain access to the server
    # Manage permitted public keys in the `authorized_keys` file
    passwordAuthentication = false;
    #  permitRootLogin = "no";
  };


  users.users.ship = {
    isNormalUser = true;
    extraGroups = [ "wheel" "nginx" ];
    # If you don't want public keys to live in the repo, you can remove the line below
    # ~/.ssh will be used instead and will not be checked into version control. 
    # Note that this requires you to manage SSH keys manually via SSH,
    # and your will need to manage authorized keys for root and ship user separately
    openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDLEEUVkQQby8rPkf3/L9e6gYuBoM9B8y+WeJEZqmQIHvjXSwvIm3nFZCRgy3sQOgqjaL0zooK0XWRA00izm899TgwM/lo2USGrbp8jybMrAQuzJvqhkAULa8JhBWuD02t53tcHdqNPUon5NFI0eB1x1XZt6KqUmtx3/39oED7DOod9wfBYdDtpIlfxzCIiG7e5UvMiP/a75vWKLGOzq2TSa7DAiaaEYfqG4FGc3W43p6UNTmrAZcnp464LozCyQIQ5Ng0cjyx6hA9gJyXcmi4QUYvauDFRfVYBwFL+FIhE+677H0PsFVjT4kOoKMNeVznWmyiek0wUuBQSnxxGHgfGY2DOtDUItPw8mUqWn2wgu6uQM9RhR6TMSeN6DOHbE0WT//H5TbEnFaUEjL+9R+2JgWIkq7XXxzWPy2FqAEjh2rvF5j8rY4clUzYOLXx6hjiduBbaUNr042AuoWvvyteul1wg0HSEcapRlYS9dy+wnDHW6AvyvAWzdIKywXrvuJU= ship@tite-ship
"
    ];
  };

  # Can be removed if you want authorized keys to only live on server, not in repository
  # Se note above for users.users.ship.openssh.authorizedKeys.keyFiles
  users.users.root.openssh.authorizedKeys.keyFiles = [ ./authorized_keys ];
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDLEEUVkQQby8rPkf3/L9e6gYuBoM9B8y+WeJEZqmQIHvjXSwvIm3nFZCRgy3sQOgqjaL0zooK0XWRA00izm899TgwM/lo2USGrbp8jybMrAQuzJvqhkAULa8JhBWuD02t53tcHdqNPUon5NFI0eB1x1XZt6KqUmtx3/39oED7DOod9wfBYdDtpIlfxzCIiG7e5UvMiP/a75vWKLGOzq2TSa7DAiaaEYfqG4FGc3W43p6UNTmrAZcnp464LozCyQIQ5Ng0cjyx6hA9gJyXcmi4QUYvauDFRfVYBwFL+FIhE+677H0PsFVjT4kOoKMNeVznWmyiek0wUuBQSnxxGHgfGY2DOtDUItPw8mUqWn2wgu6uQM9RhR6TMSeN6DOHbE0WT//H5TbEnFaUEjL+9R+2JgWIkq7XXxzWPy2FqAEjh2rvF5j8rY4clUzYOLXx6hjiduBbaUNr042AuoWvvyteul1wg0HSEcapRlYS9dy+wnDHW6AvyvAWzdIKywXrvuJU= ship@tite-ship
"
  ];

  security.sudo.extraRules = [
    {
      users = [ "ship" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" "SETENV" ];
        }
      ];
    }
  ];
}
