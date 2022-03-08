module Consts where

import Data.Text (Text)

import Data.Set (Set)
import Data.Set qualified as Set

import VK

hseUniIds :: Set UniversityId
hseUniIds = Set.fromList $ UniversityId <$> [ 128 -- Мск
                                            , 17 -- Спб
                                            , 2241 -- Пермь
                                            , 1179 -- НН
                                            ]

hseGroups :: [(Text, GroupId)]
hseGroups =
    fmap GroupId <$>
        [ ("hsemem", 139105204)
        , ("prostpolitika", 160121249)
        , ("ombudsman_hse", 203966578)
        , ("hse_overheard", 57354358)
        , ("curatorhse", 128354471)
        , ("hse_cc", 185974612)
        , ("hseapp", 150048323)
        , ("hse_digital", 203687363)
        , ("hse_social", 165826930)
        , ("studsovetgsb", 135324713)
        , ("hsenn_career", 41221467)
        , ("ideal_hse", 185550746)
        , ("permhsecouncil", 122981522)
        , ("hse_go", 37731763)
        , ("hse_mezhcampus", 89964203)
        , ("studentdiscussion", 189288871)
        , ("hsecouncil_fandom", 207486284)
        , ("bakalavrik_hse_art_and_design", 196937408)
        , ("hsedesign", 21317467)
        , ("pravohseru", 4565)
        , ("doxajournal", 128503206)
        , ("patsanskoepravo", 130311951)
        , ("vsheviypolit", 171209032)
        , ("hsecouncil", 64952366)
        , ("sociology_hse", 204807201)
        , ("hsehum", 170140130)
        , ("hse_spb", 299)
        , ("vsevyshka", 198494298)
        , ("studsovetfcmd", 110302024)
        , ("hsemedia", 30092695)
        , ("thevyshka", 66036248)
        , ("hse_career", 278573)
        , ("hseteachers", 63442801)
        , ("blogzhukov", 85283054)
        , ("tvhse", 35385290)
        , ("hsebusinessclub", 154645194)
        , ("hse_gsb", 89765286)
        , ("styleru", 45231426)
        , ("psyhse", 36286006)
        , ("hse.social", 111999555)
        , ("hseminecraft", 193111469)
        , ("nbhse", 131597813)
        , ("ueb_design", 171508908)
        , ("cpkhse", 6222726)
        , ("artclub_hse", 166280924)
        , ("tea_culture_club_hse", 204307519)
        , ("hsepubliclawclub", 201485148)
        , ("podslushanoxce", 185134520)
        , ("hsegreen", 126246699)
        , ("hseteachers", 63442801)
        , ("studsciencehse", 202400424)
        ]
