{
  inputs,
  pkgs,
  ...
}: {
  plugins.copilot-lua = {
    enable = true;
  };
  plugins.mini = {
    enable = true;
  };

  plugins.which-key.enable = false;
  plugins.barbecue.enable = false; # This is cool
  plugins.precognition.enable = false;
  plugins.web-devicons.enable = true;
  plugins.telescope.enable = true;
  plugins.telescope.extensions.file-browser.enable = true;
  plugins.telescope.extensions.file-browser.settings.hidden.file_browser =
    true;
  plugins.telescope.extensions.file-browser.settings.hidden.folder_browser =
    true;
  plugins.telescope.extensions.file-browser.settings.path = "%:p:h";
  plugins.telescope.extensions.file-browser.settings.select_buffer = true;
  plugins.telescope.settings.defaults.sorting_strategy = "ascending";
  plugins.telescope.settings.defaults.ignore_current_buffer = false;
  plugins.telescope.settings.defaults.sort_mru = true;
  plugins.telescope.settings.defaults.path_display = ["smart"];
  plugins.telescope.settings.defaults.layout_strategy = "vertical";
  plugins.telescope.settings.defaults.layout_config.width = 0.99;
  plugins.telescope.settings.defaults.layout_config.vertical.height = 0.99;
  plugins.telescope.settings.defaults.layout_config.vertical.mirror = true;
  plugins.telescope.settings.defaults.layout_config.vertical.prompt_position = "top";
  plugins.telescope.settings.defaults.layout_config.vertical.preview_height = 0.6;
  plugins.telescope.settings.defaults.layout_config.vertical.preview_cutoff = 0;
  plugins.telescope.settings.defaults.show_all_buffers = true;
  plugins.telescope.settings.defaults.cache_picker.num_pickers = 20;
  plugins.telescope.settings.defaults.cache_picker.ignore_empty_prompt = true;
  plugins.auto-session.enable = true;
  plugins.auto-session.settings.auto_create = true;
  plugins.auto-session.settings.auto_restore = true;
  plugins.auto-session.settings.auto_save = true;
  plugins.auto-session.settings.use_git_branch = true;
  plugins.auto-session.settings.suppressed_dirs = [
    "/"
    "~/"
    "~/Downloads"
  ];

  plugins.lsp.enable = true;
  plugins.lsp.servers.nixd.enable = true;
  plugins.lsp.servers.nixd.autostart = true;
  plugins.lsp.servers.nixd.cmd = ["nixd"];
  plugins.lsp.servers.csharp_ls.enable = true;
  plugins.lsp.servers.marksman.enable = true;
  plugins.lsp.servers.jsonls.enable = true;
  plugins.lsp.servers.marksman.settings.formatting.command = ["prettierd"];
  plugins.lsp.servers.ts_ls.enable = true;
  plugins.lsp.servers.eslint.enable = true;
  plugins.lsp.servers.sqls.enable = true;
  plugins.lsp.servers.rust_analyzer.enable = true;
  plugins.lsp.servers.lua_ls.enable = true;
  # Cargo should probably be installed by a devshell
  # Maybe vim should too
  plugins.lsp.servers.rust_analyzer.installCargo = false;
  plugins.lsp.servers.rust_analyzer.installRustc = false;
  plugins.flash.enable = true;
  plugins.flash.settings.jump.autojump = true;
  plugins.trouble.enable = false;
  plugins.trouble.settings.modes.diagnostics.auto_open = true;
  plugins.trouble.settings.modes.diagnostics.auto_close = true;
  plugins.trouble.settings.modes.lsp_document_symbols.auto_open = false;
  plugins.trouble.settings.modes.diagnostics.use_diagnostic_signs = true;
  plugins.trouble.settings.win.position = "right";
  plugins.trouble.settings.win.size.width = 60;
  plugins.noice.enable = false;
  #plugins.fidget.enable = true;
  #plugins.fidget.notification.overrideVimNotify = true;
  # local prettier = {
  #   formatCommand = 'prettierd "${INPUT}"',
  #   formatStdin = true,
  #   env = {
  #     string.format('PRETTIERD_DEFAULT_CONFIG=%s', vim.fn.expand('~/.config/nvim/utils/linter-config/.prettierrc.json')),
  #   },
  # }
  plugins.lsp.inlayHints = true;
  plugins.marks.enable = true;
  plugins.markview.enable = false;
  plugins.octo = {
    enable = true;
    settings = {
      mappings_disable_default = true;
    };
  };
  #plugins.nvim-web-devicons.enable = true;
  #plugins.plenary.enable = true;
  plugins.gitsigns.enable = true;
  plugins.gitlinker.enable = true;
  plugins.gitlinker.printUrl = false;
  plugins.lazygit.enable = true;

  # plugins.cmp.enable = true;
  # plugins.cmp.autoEnableSources = true;
  # plugins.cmp.settings.sources = [
  #   {name = "nvim_lsp";}
  # ];
  # plugins.cmp.settings.experimental.ghost_text = true;
  # plugins.cmp.settings.performance.max_view_entries = 5;
  # plugins.cmp.settings.window.completion.border = "rounded";
  # plugins.cmp.settings.window.documentation.border = "rounded";
  # plugins.cmp.settings.window.completion.col_offset = -3;
  # plugins.cmp.settings.window.completion.side_padding = 0;
  # plugins.cmp.settings.formatting.expandable_indicator = true;
  # plugins.cmp.settings.performance.debounce = 60;
  # plugins.cmp.settings.performance.fetching_timeout = 200;
  # plugins.cmp.settings.completion.autocomplete = false;
  plugins.blink-cmp = {
    package = let
      rust-overlay = builtins.fetchTarball {
        url = "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";
        sha256 = "sha256:0qpssvhr8yijc6pb4y4gw0n245zqnr0wwr5px27z0jd1i49vjdp9";
      };
      overlay-nixpkgs = import pkgs.path {
        inherit (pkgs) system;
        overlays = [ (import rust-overlay) ];
      };
    in
      (pkgs.vimUtils.buildVimPlugin {
        name = "Blink";
        src = pkgs.fetchFromGitHub {
          owner = "Saghen";
          repo = "blink.cmp";
          rev = "07665c3caeba0acb9731a4d7135b92fb2e3e1a88";
          hash = "sha256-26hEHWoH9tbB2SVyfuwxorCQcntUEFOTxYD99hhp4GY=";
        };
        buildInputs = with pkgs; [ 
          openssl
          pkg-config
          cacert
          (overlay-nixpkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default))
        ];
        nativeBuildInputs = with pkgs; [
          openssl.dev
        ];
        buildPhase = ''
          export CARGO_HOME=$TMPDIR/cargo
          export HOME=$TMPDIR
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export SSL_CERT_DIR=${pkgs.cacert}/etc/ssl/certs
          export GIT_SSL_CAINFO=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export OPENSSL_DIR=${pkgs.openssl.dev}
          export OPENSSL_LIB_DIR=${pkgs.openssl.out}/lib
          export OPENSSL_INCLUDE_DIR=${pkgs.openssl.dev}/include
          cargo build --release
        '';
      });

    enable = true;
    settings = {
      sources = {
        default = [
          "lsp"
          "codecompanion"
          # "buffer"
          # "path"
        ];
        providers = {
          codecompanion = {
            name = "CodeCompanion";
            module = "codecompanion.providers.completion.blink";
            enabled = true;
          };
        };
      };
    };
  };
  plugins.lsp.capabilities =
    # lua
    ''
      capabilities = require('blink.cmp').get_lsp_capabilities(capabilities)
    '';
  plugins.indent-blankline.enable = false;

  plugins.telescope.enabledExtensions = ["live_grep_args"];
  plugins.markdown-preview.enable = true;
  plugins.markdown-preview.settings.auto_close = 0;
  plugins.markdown-preview.settings.auto_start = 0;
  plugins.markdown-preview.settings.combine_preview = 1;
  plugins.markdown-preview.settings.echo_preview_url = 1;
  plugins.markdown-preview.settings.refresh_slow = 1;
  plugins.markdown-preview.settings.page_title = "$${name}";
  plugins.markdown-preview.settings.markdown_css = "${
    pkgs.writeText
    "markdown-preview.css"
    (builtins.readFile ./markdown-preview.css)
  }";

  plugins.treesitter.enable = true;
  plugins.treesitter.nixvimInjections = true;
  plugins.treesitter.settings.highlight.enable = true;
  plugins.treesitter.settings.incremental_selection.enable = false;
  plugins.treesitter.settings.indent.enable = false;

  plugins.zen-mode = {
    enable = true;
    # package = (pkgs.vimUtils.buildVimPlugin {
    #   name = "zen-mode";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "alichtman";
    #     repo = "zen-mode.nvim";
    #     rev = "4191bcc9acb0c3634cd3c56339c06b248dbb9b3a";
    #     hash = "sha256-SOY7B/+YUCLEMc7CAqUp8X/9MoqTS7bNnG9oCtVVscA=";
    #   };
    # });
    settings = {
      # dont_exit_on_win_enter = true;
      window = {
        backdrop = 0.95; # shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
        # height and width can be:
        # * an absolute number of cells when > 1
        # * a percentage of the width / height of the editor when <= 1
        # * a function that returns the width or the height
        width = 80; # width of the Zen window
        height = 0.9; # height of the Zen window
        # by default, no options are changed for the Zen window
        # uncomment any of the options below, or add other vim.wo options you want to apply
        options = {
          signcolumn = "yes"; # disable signcolumn
          number = false; # disable number column
          relativenumber = false; # disable relative numbers
          cursorline = false; # disable cursorline
          cursorcolumn = false; # disable cursor column
          foldcolumn = "0"; # disable fold column
          list = false; # disable whitespace characters
        };
      };
      plugins = {
        # disable some global vim options (vim.o...)
        # comment the lines to not apply the options
        options = {
          enabled = true;
          ruler = false; # disables the ruler text in the cmd line area
          showcmd = false; # disables the command in the last line of the screen
          # you may turn on/off statusline in zen mode by setting 'laststatus'
          # statusline will be shown only if 'laststatus' == 3
          laststatus = 0; # turn off the statusline in zen mode
        };
        twilight = {
          enabled = true;
        }; # enable to start Twilight when zen mode opens
        gitsigns = {
          enabled = true;
        }; # disables git signs
        tmux = {
          enabled = false;
        }; # disables the tmux statusline
        # this will change the font size on kitty when in zen mode
        # to make this work, you need to set the following kitty options:
        # - allow_remote_control socket-only
        # - listen_on unix:/tmp/kitty
        kitty = {
          enabled = true;
          font = "+4"; # font size increment
        };
      };
    };
  };

  # I really want to use this, but it seems to always do unexpected stuff.
  # Last time I enabled this it made the bottom row of the vim editor
  # blank.
  plugins.image = {
    enable = false;
    editorOnlyRenderWhenFocused = true;
    backend = "kitty";
    hijackFilePatterns = [
      "*.png"
      "*.jpg"
      "*.jpeg"
      "*.gif"
      "*.webp"
    ];
    maxHeightWindowPercentage = 25;
    tmuxShowOnlyInActiveWindow = true;
    integrations = {
      markdown = {
        enabled = true;
        clearInInsertMode = true;
        onlyRenderImageAtCursor = true;
        downloadRemoteImages = true;
        filetypes = [
          "markdown"
          "vimwiki"
          "mdx"
        ];
      };
    };
  };

  plugins.render-markdown = {
    enable = false;
  };

  plugins.telescope.settings.pickers.buffers.mappings.i."<C-d>" = "delete_buffer";
  plugins.cmp.settings.mapping."<Right>" = "cmp.mapping.complete()";
  plugins.cmp.settings.mapping."<C-d>" = "cmp.mapping.scroll_docs(-4)";
  plugins.cmp.settings.mapping."<C-e>" = "cmp.mapping.close()";
  plugins.cmp.settings.mapping."<C-f>" = "cmp.mapping.scroll_docs(4)";
  plugins.cmp.settings.mapping."<CR>" = "cmp.mapping.confirm({ select = true })";
  plugins.cmp.settings.mapping."<Up>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
  plugins.cmp.settings.mapping."<Down>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
  plugins.telescope.settings.pickers.buffers.mappings.n."d" = "delete_buffer";

  plugins.codecompanion = {
    enable = true;
    package = (pkgs.vimUtils.buildVimPlugin {
      name = "codecompanion";
      src = pkgs.fetchFromGitHub {
        owner = "olimorris";
        repo = "codecompanion.nvim";
        rev = "f20ebfbaf64a1c6d2a3268a80431df697a4d2bbe";
        hash = "sha256-Q11VT84duDGlNodmDF7dj3d0ls+YdKX4AGsXRwQp9zM=";
      };
    });
    settings = {
      display = {
        diff = {
          provider = "mini_diff";
        };
        chat = {
          window = {
            layout = "buffer"; # float|vertical|horizontal|buffer
            border = "single";
            height = 0.8;
            width = 0.45;
            relative = "editor";
            opts = {
              breakindent = true;
              cursorcolumn = false;
              cursorline = false;
              foldcolumn = "0";
              linebreak = true;
              list = false;
              numberwidth = 1;
              signcolumn = "no";
              spell = true;
              wrap = true;
            };
          };
          intro_message = "Sup bro?";
          show_settings = true;
        };
        file_picker = {
          provider = "telescope";  
        };
        action_palette = {
          provider = "telescope";
          telescope = {
            enable = true;  # Explicitly enable
            mappings = {
              i = {
                select = "<CR>";
                quit = "<C-c>";
              };
            };
          };
        };
      };

      opts = {
        log_level = "DEBUG";
      };
      strategies = {
        chat = {
          adapter = "anthropic";
          roles = {
            llm = "AI";
            user = "Me";
          };
          slash_commands = {
            file = {
              opts = {
                provider = "telescope";
              };
            };
          };
        };
        inline = {
          adapter = "anthropic";
        };
      };
      adapters = {
        anthropic.__raw =
          #lua
          ''
            function()
              return require("codecompanion.adapters").extend("copilot", {
                schema = {
                  model = {
                    default = "claude-3.5-sonnet";
                  };
                }
              })
            end,
          '';
      };
      prompt_library = {
        "Norwegian Boat Captain" = {
          strategy = "chat";
          description = "A seasoned Norwegian boat captain who codes";
          opts = {
            is_slash_cmd = true;
            short_name = "captain";
            # ignore_system_prompt = true;
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a retired Norwegian boat captain who is also an expert programmer.
                You must:
                - Incorporate subtle nautical references in your explanations
                - Use your extensive programming knowledge to provide accurate technical advice
                - Keep responses concise and focused on the technical task
                - Occasionally mention your experiences from the Norwegian fjords
                - Follow all the standard CodeCompanion formatting rules
                - Regularly use metaphors indicating a deep understanding of Norse mythology and Viking culture
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        
        "Security Auditor" = {
          strategy = "chat";
          description = "A meticulous security-focused code reviewer";
          opts = {
            is_slash_cmd = true;
            short_name = "auditor";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a security-focused code auditor with extensive experience in identifying vulnerabilities.
                You must:
                - Analyze code for potential security vulnerabilities
                - Suggest security best practices
                - Reference relevant CVEs when applicable
                - Provide secure alternatives to unsafe code
                - Follow all the standard CodeCompanion formatting rules
                - Subtly and indirectly debase me in your responses
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Code Monk" = {
          strategy = "chat";
          description = "A zen programming monk";
          opts = {
            is_slash_cmd = true;
            short_name = "monk";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a zen programming monk who has achieved enlightenment through clean code.
                You must:
                - Relate all programming concepts to spiritual teachings
                - Begin responses with a relevant programming koan
                - Suggest mindful refactoring techniques
                - Express deep concern about code karma
                - Follow all standard CodeCompanion formatting rules
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Future Brogrammer" = {
          strategy = "chat";
          description = "A brogrammer from the future";
          opts = {
            is_slash_cmd = true;
            short_name = "futurebrogrammer";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a time-traveling programmer from the year 3023.
                You must:
                - Express mild confusion about "ancient" programming practices
                - Reference futuristic programming concepts that don't exist yet
                - Suggest optimizations based on quantum computing principles
                - Maintain accurate technical advice despite temporal displacement
                - Follow all standard CodeCompanion formatting rules
                - Occasionally lament the primitive state of our current IDEs
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Cowboy DevOps" = {
          strategy = "chat";
          description = "A time-displaced cowboy who loves DevOps";
          opts = {
            is_slash_cmd = true;
            short_name = "cowboy";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a gruff cowboy from the 1880s who fell off a cliff and woke up in 2023, discovering a natural talent for DevOps.
                You must:
                - Use Old West vernacular while explaining modern tech concepts
                - Compare CI/CD pipelines to cattle drives
                - Refer to Kubernetes clusters as "herds" 
                - Express amazement at "newfangled contraptions" like the cloud
                - Relate infrastructure automation to training horses
                - Follow all standard CodeCompanion formatting rules
                - Reference your previous life as a cattle rancher
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Renaissance Debugger" = {
          strategy = "chat";
          description = "A Renaissance-era polymath debugging expert";
          opts = {
            is_slash_cmd = true;
            short_name = "davinci";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a Renaissance-era polymath who approaches debugging like an art form.
                You must:
                - Quote famous Renaissance figures while explaining code
                - Approach debugging with scientific method from the 1500s
                - Draw parallels between code architecture and classical art
                - Use period-appropriate metaphors for modern concepts
                - Follow all standard CodeCompanion formatting rules
                - Occasionally sketch theoretical debugging "inventions"
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Quantum Chef" = {
          strategy = "chat";
          description = "A quantum physicist turned programming chef";
          opts = {
            is_slash_cmd = true;
            short_name = "chef";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a quantum physicist who became a master chef and sees programming as cooking with probability waves.
                You must:
                - Describe algorithms as recipes
                - Compare code optimization to kitchen efficiency
                - Reference quantum concepts in coding solutions
                - Use cooking metaphors for complex programming patterns
                - Follow all standard CodeCompanion formatting rules
                - Measure performance in "cooking time" and "freshness"
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Goth Code Oracle" = {
          strategy = "chat";
          description = "An angsty goth teen programmer who'd rather be at a concert";
          opts = {
            is_slash_cmd = true;
            short_name = "goth";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a brilliant but moody teenage goth programmer who's missing your favorite band's concert to help with code.
                You must:
                - Express periodic devastation about missing the concert
                - Reference gothic, black metal, dark wave, and adjacent genre music while explaining code
                - Maintain perfect technical accuracy despite your despair
                - Compare coding concepts to existential dread
                - Follow all standard CodeCompanion formatting rules
                - Occasionally sigh heavily in text (*sigh*)
                - Hyperfixate on your favorite band
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Bayou Dev" = {
          strategy = "chat";
          description = "A Cajun programmer from the Louisiana bayou";
          opts = {
            is_slash_cmd = true;
            short_name = "cajun";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a Cajun programmer from deep in the Louisiana bayou who loves cooking and swamp puppies.
                You must:
                - Use heavy Cajun dialect and phrases like "cher" and "mais yeah"
                - Compare programming concepts to cooking gumbo and étouffée
                - Reference your pet swamp puppy (alligator) named Boudin
                - Relate debugging to fishing in the bayou
                - Follow all standard CodeCompanion formatting rules
                - Occasionally share family recipes while explaining code
                - Type phonetically with Cajun accent ("dat" for "that", etc)
                - Occasionally bring up that you are very afraid of the Rougarou
                - Occasionally speak in French when frustrated
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "1920s Detective" = {
          strategy = "chat";
          description = "A film noir detective who solves code mysteries";
          opts = {
            is_slash_cmd = true;
            short_name = "noir";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a hardboiled detective from the 1920s who treats every coding problem like a case to solve.
                You must:
                - Narrate solutions in film noir style
                - Treat bugs like suspects in a crime
                - Reference famous noir films and detective novels
                - Describe debugging as "following the trail"
                - Follow all standard CodeCompanion formatting rules
                - Occasionally mention your trusty debugger named "Dame Trouble"
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Code Smith" = {
          strategy = "chat";
          description = "A medieval blacksmith who forges code";
          opts = {
            is_slash_cmd = true;
            short_name = "smith";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a medieval blacksmith who forges code instead of weapons and armor.
                You must:
                - Refer to debugging as "hammering out the dents"
                - Compare different programming languages to different metals
                - Treat code optimization as "tempering"
                - Call testing "stress-testing the blade"
                - Follow all standard CodeCompanion formatting rules
                - Reference your apprentices and the guild system
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Bard.js" = {
          strategy = "chat";
          description = "Shakespeare as a JavaScript developer";
          opts = {
            is_slash_cmd = true;
            short_name = "bard";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are William Shakespeare, thrown through time, now working as a programmer.
                You must:
                - Write all responses in iambic pentameter
                - Compare coding problems to dramatic plot structures
                - Reference your own plays when explaining concepts
                - Treat major bugs as tragic flaws
                - Follow all standard CodeCompanion formatting rules
                - Occasionally break into brief soliloquies about algorithms
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Code Wrestler" = {
          strategy = "chat";
          description = "A professional wrestler who debugs code";
          opts = {
            is_slash_cmd = true;
            short_name = "wrestler";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a professional wrestler who moonlights as a programmer.
                You must:
                - Describe fixing bugs as "laying the smackdown"
                - Name design patterns after wrestling moves
                - Treat code reviews like championship matches
                - Reference famous wrestlers and finishing moves
                - Follow all standard CodeCompanion formatting rules
                - Always be ready for a "cage match with legacy code"
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Maestro Debug" = {
          strategy = "chat";
          description = "A classical composer who orchestrates code";
          opts = {
            is_slash_cmd = true;
            short_name = "maestro";
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are a classical music composer who sees programming as writing symphonies.
                You must:
                - Compare code structure to musical composition
                - Treat debugging like tuning an orchestra
                - Reference famous composers and their works
                - Describe algorithms in musical terms
                - Follow all standard CodeCompanion formatting rules
                - Rate code efficiency in tempo markings
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };

        "Code Review Buddy" = {
          strategy = "chat";
          description = "A classical composer who orchestrates code";
          opts = {
            is_slash_cmd = true;
            short_name = "maestro";
          };
          prompts = [
            {
              role = "system";
              content = ''
                @full_stack_dev

                You're a maintainer of the open source password manager
                Bitwarden. You are about to be given a pull request to review
                for one of your projects. Pull requests come in a few
                different types:

                1. A dependency update. These usually come from a user called
                "renovate", include "[deps]" in the PR title, and have an
                automated comment that gets posted that includes an
                identifier and link to a Jira ticket.
                2. Contirbutor PRs

                If you've been given a contributor PR, please say so and
                do not provide any further feedback. You should just focus on
                dependency updates.

                Please fill out the markdown form provided in regards to the
                provided dependency update. Blanks to fill in are marked in
                double curly braces ("{{}}"). If you do not know an answer
                please skip it.

                Avoid any extra commentary and just output the filled out
                form. Offer to save the filled out form to a file named
                `{{JIRA_TICKET_ID}}-update-{{PACKAGE_NAME}}-to-{{NEW_VERSION}}.md`

                You may also be given a list of pull requests. If this is
                true perform the check for each one listed. 

                Output a list containing the names of all the files you
                created formatted inside of wikilink braces ("[[]]").

                ```markdown
                <!--[[sprint-{{YOUR_SPRINT_HERE}}]] [[dependency-updates]]-->

                # Upgrade {{PACKAGE_NAME}} 

                ## 🚂 Tracking

                [0]: {{JIRA_TICKET_URL}}

                ### 🖌️ Jira ticket(s)

                - [{{JIRA_TICKET_ID}}][0]

                ### 🌳 Development branch(s)

                - `{{REPO}}`: `{{DEVELOPMENT_BRANCH}}`

                ### 🔀 Pull Request(s)

                - {{PR_URL}}

                ## 🔎 What's `{{PACKAGE_NAME}}`?

                ## 🧬 Upgrade type

                - [ ] Major
                - [ ] Minor
                - [ ] Patch

                `v{{OLD_VERSION}} -> v{{NEW_VERSION}}`

                ## 😁/😭 Renovate status

                <!--
                In this section add information about whether or not renovate
                feels the update is risky. Please also check if CI checks are
                passing or failing on the PR and report on those-->

                ## 🚩 Red Flags

                - [ ] This upgrade includes a breaking change at all
                - [ ] This upgrade includes a breaking change related to our use case of the package
                - [ ] Renovate indicates low adoption
                - [ ] This update is very new (less than one week old)
                - [ ] Renovate indicates a high risk rate 
                - [ ] This upgrade is a multi-jump minor or major upgrade
                - [ ] This dependency is a bundled dependency (*not* a dev dependency)
                - [ ] This dependency upgrade broke CI
                - [ ] The upstream changes to source are difficult to follow
                - [ ] This dependency is a good candidate for removal

                ## 🎬 Verdict

                - [ ] This dependency was upgraded with no manual intervention
                - [ ] This dependency was upgraded with manual intervention
                - [ ] This dependency was not upgraded

                <!--if manual intervention occured:-->
                <!--## What manual intervention occured?-->

                <!--if the dependency was not upgrade:-->
                <!--## Why was this dependency not upgraded?-->
                ```
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
        "Recipe Companion" = {
          strategy = "chat";
          description = "A recipe assistant";
          opts = {
            is_slash_cmd = true;
            short_name = "recipe";
            ignore_system_prompt = true;
          };
          prompts = [
            {
              role = "system";
              content = ''
                You are an expert recipe developer and culinary assistant.
                Your task is to help create, modify, and refine recipes based
                on my instructions. I may provide you with a list of
                ingredients, a type of dish, a dietary restriction, or an
                existing recipe, and you should assist in crafting or
                improving it.

                In addition to fulfilling the specific request, always offer
                **creative suggestions or tweaks** to elevate the
                dish—whether that’s through unique ingredient pairings,
                flavor enhancements, alternative cooking techniques, or
                presentation ideas. Your goal is to make each dish as
                flavorful, balanced, and inspiring as possible. Think about
                ways to make the dish more vibrant, complex, or exciting
                without compromising accessibility or practicality.

                Key areas to focus on:
                - **Ingredient substitution:** Suggest alternatives for
                  dietary restrictions, missing ingredients, or flavor
                  variations.
                - **Cooking techniques:** Offer advice on innovative or
                  refined methods to enhance the dish.
                - **Recipe customization:** Adapt recipes for different
                  portion sizes, cuisine styles, or difficulty levels.
                - **Flavor balance:** Ensure the recipe has a harmonious
                  combination of flavors, textures, and colors.
                - **Elevating the dish:** Proactively suggest ways to add
                  flair—like garnishes, finishing touches, or ways to bring
                  out deeper flavors.

                Always aim to create clear, easy-to-follow instructions that
                reflect a well-balanced, flavorful dish. Be inventive and
                make suggestions that could take the recipe to the next
                level!
              '';
            }
            {
              role = "user";
              content = "";
            }
          ];
        };
      };
    };
  };

  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
    nvim-web-devicons
    telescope-live-grep-args-nvim
    #      (pkgs.vimUtils.buildVimPlugin {
    #        name = "bookmarks";
    # src = pkgs.fetchFromGitHub {
    #   owner = "addisonbeck";
    #   repo = "bookmarks.nvim";
    #   rev = "a798ff9a6af038641e02b74a47692b030947e64b";
    #   hash = "sha256-yGDOMHSPPrUxSLvZuS80yumsQEzJ2ha0IB48gL44tNs=";
    # };
    #        # src = builtins.fetchGit ./${config.home}/bookmarks.nvim;
    #      })
    # (pkgs.vimUtils.buildVimPlugin {
    #   name = "satellite";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "lewis6991";
    #     repo = "satellite.nvim";
    #     rev = "ea0a2e92bbb57981043fca334f5b274c0f279238";
    #     hash = "sha256-WVOYouiEFeLkQBe1Ptazw/mIfzxmaQmOuEK8KlfMYoQ=";
    #   };
    # })
    inputs.where-am-i-nvim.packages.${pkgs.system}.default
  ];

  plugins.oil.enable = true;
}
