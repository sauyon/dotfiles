{ config, lib, pkgs, ... }:

{
  xdg.configFile."opencode/opencode.json".text = builtins.toJSON {
    "$schema" = "https://opencode.ai/config.json";
    provider = {
      mcloud = {
        npm = "@ai-sdk/openai-compatible";
        name = "Modular MAX Cloud";
        options = {
          baseURL = "https://model.api.modular.com/v1";
        };
        models = { };
      };
      "ko-ag" = {
        npm = "@ai-sdk/openai-compatible";
        name = "ko.ag";
        options = {
          baseURL = "https://ai.ko.ag/v1";
        };
        models = {
          "gemma4:31b" = {
            name = "Gemma 4 31B";
          };
        };
      };
    };
  };
}
