module SiteDefinition.Types where

import Data.Text (Text)
import Data.Map (Map (..))

{-
 TODO:
 * Add:
  - markup content
  - themes (layouts + support data to implement a look)
  - templates (rendering sources (html, xml, ...))
  - assets
  - data sources
  - static resources + ?destination?
  - other resources
  - global info context (extracted from config files?)

 * Create:
  - WorkPlan: description of what needs to be done to each (MarkupPage, [Templates]) (runtime engine, parameters).
  - Transformation: description of what needs to be done to output the content in the right format.
-}

{-
Directories: 
  - archetypes : The archetypes directory contains templates for new content. See details.
  - assets : The assets directory contains global resources typically passed through an asset
          pipeline. This includes resources such as images, CSS, Sass, JavaScript, and TypeScript. See details.
  - config : The config directory contains your site configuration, possibly split into multiple
          subdirectories and files. For projects with minimal configuration or projects that do not
          need to behave differently in different environments, a single configuration file named hugo.toml
          in the root of the project is sufficient. See details.
  - content : The content directory contains the markup files (typically markdown) and page resources
          that comprise the content of your site. See details.
  - data : The data directory contains data files (JSON, TOML, YAML, or XML) that augment content,
          configuration, localization, and navigation. See details.
  - i18n : The i18n directory contains translation tables for multilingual sites. See details.
  - layouts : The layouts directory contains templates to transform content, data, and resources into
          a complete website. See details.
  - public : The public directory contains the published website, generated when you run the hugo command.
          Hugo recreates this directory and its content as needed. See details.
  - resources : The resources directory contains cached output from Hugo’s asset pipelines, generated when
          you run the hugo or hugo server commands. By default this cache directory includes CSS and images.
          Hugo recreates this directory and its content as needed.
  - static : The static directory contains files that will be copied to the public directory when you build
          your site. For example: favicon.ico, robots.txt, and files that verify site ownership. Before the
          introduction of page bundles and asset pipelines, the static directory was also used for images, CSS,
          and JavaScript. See details.
  - themes : The themes directory contains one or more themes, each in its own subdirectory.
-}


data SiteEntry a = SiteEntry {
    path :: FilePath
    , deref :: Maybe a
  }


data MarkupEntry = MarkupEntry
data ThemeEntry = ThemeEntry
data TemplateEntry = TemplateEntry
data AssetEntry = AssetEntry
data DataSrcEntry = DataSrcEntry
data ResourceEntry = ResourceEntry
data ConfigEntry = ConfigEntry

data SiteDefinition = SiteDefinition {
    baseDir :: String
    , markupContent :: [ SiteEntry MarkupEntry ]
    , themes :: [ SiteEntry ThemeEntry ]
    , templates :: [ SiteEntry TemplateEntry ]
    , assets :: [ SiteEntry AssetEntry ]
    , dataSources :: [ SiteEntry DataSrcEntry ]
    , resources :: [ SiteEntry ResourceEntry ]
    , staticDest :: FilePath
    , configs :: [ SiteEntry ConfigEntry ]
  }

data DirectoryKind =
  Archetypes
 | Assets
 | Config
 | Content
 | Data
 | I18n
 | Layouts
 | Public
 | Resources
 | Static
 | Themes


kindToName :: DirectoryKind -> String
kindToName aKind =
  case aKind of
    Archetypes -> "archetypes"
    Assets -> "assets"
    Config -> "config"
    Content -> "content"
    Data -> "data"
    I18n -> "i18n"
    Layouts -> "layouts"
    Public -> "public"
    Resources -> "resources"
    Static -> "static"
    Themes -> "themes"


nameToKind :: String -> Either String DirectoryKind
nameToKind aName =
  case aName of
    "archetypes" -> Right Archetypes
    "assets" -> Right Assets
    "config" -> Right Config
    "content" -> Right Content
    "data" -> Right Data
    "i18n" -> Right I18n
    "layouts" -> Right Layouts
    "public" -> Right Public
    "resources" -> Right Resources
    "static" -> Right Static
    "themes" -> Right Themes
    _ -> Left $ "@[nameToKind] unknown name: " <> aName <> "."

-- JSON definition from LLM:
data ProjectOverview = ProjectOverview { 
    overview :: Text
    , purpose :: Text
    , goal :: Text
    , websiteStructure :: WebsiteStructure
    , designAesthetics :: Text
    , endNote :: Text
  }
  deriving (Show, Generic, FromJSON)

data WebsiteStructure = WebsiteStructure {
    homepage :: Section
    , forum :: Section
    , blogSection :: Section
    , tutorialsGuides :: Section
    , resources :: Section
    , communitySpotlight :: Section
    , eventsCalendar :: Section
    , integrationFunctionality :: Section
  }
  deriving (Show, Generic, FromJSON)

data Section = Section {
    description :: Text
    , features :: [Text]
  }
  deriving (Show, Generic, FromJSON)
