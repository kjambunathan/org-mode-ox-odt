#!/bin/sh
git cherry-pick 9d81860593dd12a8ee6ce9637fe4e4b92d4cf19e # Preparing to merge my ox-odt changes to Orgmode.org master
git cherry-pick 06bfe66aec27459994c3eb5e40db0cec40823f17 # ox-odt.el: Revert to 8ad20618d1ff574406dc1ad6e998f400
git cherry-pick c06478c9d99be23ecfda9b33edb33bad860659fd # Revert "ox-odt.el (org-odt-table-style-format): Use %s for inserting the rel-width property as a string"
git cherry-pick 8071ce913c1a34b1cbfed4920066d4d2b714c68f # Revert "Fix ox-odt bugs"
git cherry-pick be9840d204e37939b354c2361e1a6cbee2bd474a # ox-odt: Support for typesetting Description lists as in LaTeX
git cherry-pick f1e4f96edc0228fdbcb7db544cf00d72530ed7f1 # ox-odt.el: Fix handling of ODT attributes
git cherry-pick 92f0ee559d166cb9539f17134373f50cd332cab3 # ox-odt.el: Support for pagebreaks, custom paragraph styles.
git cherry-pick 6fc6c7468473a7e03f18cbd9fe53cb902c9770ef # ox-odt.el: Specify cell widths using `:widths' attribute
git cherry-pick 4578504b92175733bde483a2e13f26691852d752 # ox-odt.el: Don't allow table row to split across pages
git cherry-pick 271212243d8923297c494588c9ef5a7c1c7fdb8d # ox-odt.el: Support hrule and table attributes in list tables
git cherry-pick 4fa8daa03c6461c15a7990a2209cbb2a88eed436 # contrib/lisp/ox-freemind.el: Require cl
git cherry-pick 40bfe609ed5e71ef1ee3365eedb0b363135fe386 # ox-odt.el: Explicitly set coding system of XML files to 'utf-8
git cherry-pick 8072610932d29be81bd1af3a76b2da504d969c66 # ox-odt.el: Fix issues seen with ODT files having multiple images
git cherry-pick 65d7400a9472d175fd83107564172c9d6d59ee74 # ox-odt.el: Fix paths of data and styles directories
git cherry-pick 2d25f217e11a2bf22837acad082f6d80883238fe # ox-odt.el (generated-autoload-file): Don't set it
git cherry-pick b5afbc2f459bc06cc144649848efa659bf1ee606 # ox-odt.el (org-odt-schema-dir-list): Add git and elpa paths
git cherry-pick c51f4ba71ccaa08b8723b2acfe77ba9d46d12a53 # ox-odt.el: Fix cell borders
git cherry-pick befa2c8b3d7dbefb5d539c4c17624d8899d56870 # ox-odt.el: Cope with empty title
git cherry-pick d0fe9e23b53dae59e643d6f27d69f7b1f3c17d77 # ox-odt.el: Support for page nos. etc as part of xref-es
git cherry-pick 1bf39dfd76f8cd29b04495fe698d6b3dab126311 # Use a "OrgPageBreakDefault" as default PAGEBREAK style
git cherry-pick b2a15b5c383624e1a2f77f57ceae95d43190b586 # ox-odt.el: Support for custom para style within SPECIAL-BLOCKs
git cherry-pick 99008bbb89f5e3908ea78a6eacee7710cc461168 # Correctly check for standard-value of `org-odt-caption-and-xref-settings'.
git cherry-pick 82ea45464dfc7dea63c8a4e4c0e44c2bfd3e987f # ox-odt: Use `org-element-lineage' instead of `org-export-get-genealogy'
git cherry-pick d105d1e5746ec69778648288c1b5a5ccdff79651 # ox-odt: Fix invocation of `org-format-latex'
git cherry-pick 19723fd47abef8272477e91e17489ca7f3842825 # ox-odt.el (org-odt-special-block): Preserve case
git cherry-pick 4b8ffd3c20b671bae50b8728b3ca74bc283a4308 # ox-odt.el: Remove some export options associated to variables
git cherry-pick 0a7ced81c391e20c857d8b5ce8eb912bd54e5965 # ox-odt.el: Use options instead of hard-coded variables
git cherry-pick ee2d0e43aeebe647b61a7bb9315630c0eeac8ebd # ox-odt: Implement `org-odt-format-{headline, inlinetask}-default-function'
git cherry-pick d6e349ec787755a8fdbd4c35d0041383842c056a # ox-odt.el: Misc. changes
git cherry-pick cb1f884ee690717ed0194804c7bad2263f05cfc9 # ox-odt.el: Fix exporting radio link with missing radio target
git cherry-pick 5b1989cba2acb4f4dfbae0ac841331e19de35c37 # ox-odt.el (org-odt-link): Use `org-export-custom-protocol-maybe'.
git cherry-pick 11dbb5a3581c95a0b99b30e36963a6ef0b76b36c # ox-odt.el: Support `:with-title'
git cherry-pick 4c40f0109f059a00e815841d88c14c9c1aec2f1d # ox-odt.el: Support `:with-title'
git cherry-pick b577fdd5f2aa38506d2c06314b6841b26cb52986 # ox-odt.el: Enumerate footnotes based on body text
git cherry-pick 1d7dd0ab565eea48bbfd287d55138c79e1e412b6 # ox-odt.el: Update copyright years
git cherry-pick d84859213045a552e12e519c7ed01522d903db88 # ox-odt.el: Use scope param in org-export-footnote-* calls
git cherry-pick 96b4bbb568a473844b8ed1d6180c5eb38af7f157 # ox-odt.el (org-odt-link): Fix custom link handling
git cherry-pick 0f0513e07ef9312e4cdf0cff9de50fd563e5dde6 # ox-odt.el (org-odt-template): Don't create empty paragraphs
git cherry-pick b5e1d0b85588d81824348f229090525963225a60 # ox-odt.el: Preserve white space when converting LaTeX code.
git cherry-pick b3eedee14c5665c4102faa3e886d333196df1d15 # ox-odt.el: Fix leading whitespace
# git cherry-pick 2922956cd3440484a46b4ef340aab014c2da2e8e # org-element: Add basic citation support
# git cherry-pick fbe3059c74132938b76a553c85599c86cfe82ede # Remove @ from the :key property of citations
# git cherry-pick 7dd98d9c8db6f0b4a1d241e60be600e02239e7a1 # org-element: Fix `org-element-citation-interpreter'
# git cherry-pick 7d19a295bd9b356f377af9a747d82230ebd0066a # org-element: Change syntax for citation keys
# git cherry-pick bab686d56560cb1dde78bdf329cede7791b6a636 # org-element: Allow any word character in citation keys
# git cherry-pick 2a357f2f7da8d5b0eb5424509b8948995f9fa04f # org-element: Parse multi citations
# git cherry-pick 3a6ae8034e55b9dd150ae8e78f72c9e10bd0197c # org-element: Split citations and citation-references objects
# git cherry-pick 8ae8017afa5224fd7a9f03ca1081dbdd4780fff3 # org-element: Fix typo
# git cherry-pick 5f5cd2ee77d3de636a7f7a005f871f0768accc4d # org-element: Get rid of compiler warning
# git cherry-pick 905d6ba8034bc5b1b2fa052d2197e0edce8d4896 # ox-odt.el: Add support for JabRef
# git cherry-pick df47e82848379451f51a2d156d169b30f0186e53 # lisp/ox-odt.el: Use lexical binding
# git cherry-pick 32618d8025392b411b21062087e7b37b87a254bf # lisp/ox-odt.el: Use `cl-lib'
# git cherry-pick 9ddb3a9f73c9796d1925abe128e50a610386d129 # lisp/ox-odt.el: Add kludge to get past compilation errors
# git cherry-pick ea8415d73f545671abee66ee6c7e14dd1cd55981 # * lisp/ox-odt.el: Remove `:export-block' from backend definition
# git cherry-pick b3c2eebf374ff841abea972ff7c090bddc273913 # lisp/ox-odt.el: Manually merge some changes from Orgmode.org master
# git cherry-pick 70e13d6b9a2d876a19b94974dfbf025ff0efd596 # README.org: Note down patches and commits that are to be merged
# git cherry-pick 4ff09d4ef8758991d6da33654b07f43639d029d7 # Update README.org
# git cherry-pick 962201364d9eac96479da3b5b8b3ab23fe575449 # Update README.org
# git cherry-pick 401e7b01bd8c0fee2f3f53205620899bf7c8dcb4 # Update README.org
# git cherry-pick 8b0a9f96a90661ffe87d0b7449c04312b5aad564 # Update README.org
# git cherry-pick b50b486be0caad0d664253b65bf1e194dad7fd82 # Update README.org
# git cherry-pick 24574bbac348ae97b3c0539c75ba7ea4e72f3dd6 # lisp/ox-odt.el: Improve list tables
# git cherry-pick 0adf1601c1c38e05a7e3571b9f90510e8a968205 # Fix all loose ends with ODT's JabRef exporter
# git cherry-pick facf68b55387b44675906a48e48f6ca855d98d28 # Create todo1, done1 subdirs
# git cherry-pick 664bea04bcd54a6afe2bf033a4889b796f314060 # README.org: Update
# git cherry-pick 341fe82a2b90f5f2bbda154838c7d8ff9cd5ae14 # done2: Add directory
# git cherry-pick 850a66178d104339336cab725bf5755ea66659c8 # lisp/ox-odt.el: Support \cite{} fragments
# git cherry-pick 3ad260279ddb228aaf6e1827b652dcb938f68e77 # Copy over mathtoweb.jar for convenience
# git cherry-pick 986579726d0e2286ccde7a1032c9e9e59795b461 # Import biblatex-examples.bib
# git cherry-pick 832ad5ec36801db21fb5c16ceef79fa60cd8ef41 # Add an org file for unit testing the ODT exporter
# git cherry-pick 0fc175045d9ae6c3cd7acd51246009b69b6d84c3 # Update test-new.org
# git cherry-pick 60f0d92379d2f349c00f168b02f7b012daa649d3 # Add JabRef layout files for exporting citations in OD format
# git cherry-pick 6997cfe1a28cd9acb07bcc9c4c45d3644fdb1896 # Mark mathtoweb.jar as an executable
# git cherry-pick 49e98625df718fd1822692cfd3c1fe7ee135f15b # todo2/: Record commits and patches from that master that may have to be merged here
# git cherry-pick 6176d47d21b5085c9d9f4bf6c0b21b42638d2f12 # contrib/JabRefChicagoForOrgmode/*: Add layout files for ODF
# git cherry-pick 81957977aef990e690aed156c9bd09667a39b8fc # OrgOdtStyles.xml: Add styles for indices
# git cherry-pick 9eea0f138ec591312da3ed57d3761c92d0bb19a2 # lisp/ox-odt.el: Generate table of figures, listings and tables.
# git cherry-pick dc25160e63601591f7547c527627db7fe5b1c3b0 # testing/examples/odt/test-new.org: Test cases for TOC generation
# git cherry-pick 3e2f66a7eb67a1683b24f0282662b32effc029b0 # Move files around
# git cherry-pick d471baa13415eceb5bed29ad52a48a28cf8ae4b6 # lisp/ox-odt.el: Support document transformation using LibreOffice macros
# git cherry-pick 6dee2e434ca348e17b342d015cec7c44a53d4d9e # OrgModeUtilities.oxt: Library of LibreOffice macros
# git cherry-pick 6045ce5b96fd252d12d1ec49c16dad79bf0276a2 # testing/examples/odt/test-new.org:  Misc. changes
# git cherry-pick dd80a8e10e89ac0ac8ff63961c197b065a726a8e # * testing/examples/odt/test-new.odt: Sample document created with ODT exporter
# git cherry-pick c40b5b2641e0cd4753b810cd19cda5c29c587367 # contrib/odt/OrgModeUtilities.oxt (OptimizeColumnWidth): New
# git cherry-pick 364281914f0a41e8f0fca8ba4194ac01b47b9f58 # * lisp/ox-odt.el (org-odt-transform-processes): Add a new transformer
# git cherry-pick 9148fd363b387c2ba212c2c750b056a4a8c6996c # lisp/ox-odt.el: Honor "local" spec in "#+TOC: ..." directive
# git cherry-pick 85331655769c05c3f88baa3a69222ad7574149b7 # testing/examples/odt/test-new.org: Add a few "#+TOC: ... local" lines
# git cherry-pick 0b446370bdeac279b65761b550be9ceb3416c568 # lisp/ox-odt.el (org-odt-template):  Don't emit empty paragraphs
# git cherry-pick 5c243da2d8b297be68c863177b738564df790fe8 # lisp/ox-odt.el: Support column width and sizing of list tables
# git cherry-pick 46553d0d1ffd9c79be9f23e6a628db5971f393e8 # testing/examples/odt/test-new.org: Add col. width & align attrs to list table
# git cherry-pick 00c05082e3b80b7463e321ba2426a5a33463731a # lisp/ox-odt.el (org-odt--translate-list-tables): Fix regexp
# git cherry-pick 3355089d0a9b442040da30d0d422110532995ab7 # * lisp/ox-odt.el: Minor refactoring
# git cherry-pick dd10d04f2cdfdb0e27155c239a3ac094bb0f2e88 # lisp/ox-odt.el: Customize table paragraphs through  `:p-style' attribute
# git cherry-pick 851b588dd80b5d59caa6ea9972cf2e6a54eb3620 # lisp/ox-odt.el: In list tables, ignore col alignment cookies
# git cherry-pick f6cebf79135296f88539da517113faac1e62df0c # Vaidheeswaran.C.974555.EMACS.pdf: My GNU Emacs assignment dtd. Apr 6, 2015
# git cherry-pick 0100a7dd46239e345069efb1b8b3b535aef172e9 # Vaidheeswaran.C.974555.EMACS.pdf:  Not an executable
# git cherry-pick c397ae4f69e2ba2fefd0c4284cdf97c8d834b873 # lisp/ox-odt.el: Allow custom paragraph styles for verse blocks
# git cherry-pick 7b2484ed4029a0e2f40514117e606c3cfbee11ca # lisp/ox-odt.el: Replace `org-odt-automatic-styles' with `org-odt--automatic-styles'
# git cherry-pick 1329f6524e4f4565a8284d5cc83c1b090a48fb1b # Revert "lisp/ox-odt.el: Replace `org-odt-automatic-styles' with `org-odt--automatic-styles'"
# git cherry-pick f983aa8ba0c55a4cf4376d941f418c7c5d664611 # ox-odt.el, ox-jabref.el: Gracefully handle corner cases
# git cherry-pick 25c97445d75770565f22a5a45319fa39e7689912 # contrib/lisp/ox-jabref.el: Check for missing export plugins
# git cherry-pick 06ac3257fbd85a0de9ee635ddb674d87a69b3f31 # lisp/ox-odt.el:  Handle case where #+ODT_STYLES_FILE is empty
# git cherry-pick 39b6bc6fa6607b13f9d00e611d04b97bf2369e70 # Introduce keywords BIB_FILE and ODT_JABREF_CITATION_STYLE
# git cherry-pick 7a5b618e6ade86cd4682260989c8391b846909ce # testing/examples/odt/test-new.org:  Update
# git cherry-pick 0c91980f42e754c615da357071744b6e686670aa # contrib/odt/JabRefChicagoForOrgmode: Remove verbatim copy of this sub-project
# git cherry-pick ed734b55678a7e732e4822fbcf56d7cd2fa2abc8 # contrib/odt/JabRefChicagoForOrgmode: New submodule
# git cherry-pick 0804b34d1e9d50ca9b3ba095edd1ee7d2bda005c # Summary: contrib/odt/JabRefChicagoForOrgmode: Update
# git cherry-pick fc464a417c1aa6a6926cb864287bd66043addf1e # contrib/odt/JabRefChicagoForOrgmode: Update
# git cherry-pick 499057dafc5e0b31d4d48b5bd61bf76b8b7c5a6b # lisp/ox-odt.el:  Allow \cite{} in captions
# git cherry-pick 58755fa15ddfeb8d985c916ba8bcb9609085e4c3 # lisp/ox-odt.el: Support `#+ODT_AUTOMATIC_STYLES: ...'
# git cherry-pick 1ea275f423493af4d66b1c5c622de9c9d0a00425 # lisp/ox-odt.el: Support OpenOffice sections
# git cherry-pick a11d67b60d5714525db056c3bf71317214f36265 # lisp/ox-odt.el: Handle relative filepaths correctly
# git cherry-pick 0e1a729e20f79ed1161ed619ca7c15a258b16cd0 # * testing/examples/odt/test-new.org: Update
# git cherry-pick d1595357ec9d645e6b2c9eae25f163a2fcbb9a05 # testing/examples/odt/masterdocument.org: Renamed from test-new.org
# git cherry-pick 6f01d337fe6d6ee4fa48d89751e23fc56509f93e # * testing/examples/odt/masterdocument.org: Add snippets for transcluding ODT links * testing/examples/odt/subdocument1.org: New. * testing/examples/odt/subdocument1.odt: New. * testing/examples/odt/subdocument2.org: Update
# git cherry-pick 2fe725dfb1986ce0812f5e9957939c7874930f98 # lisp/ox-odt.el: Transclude ODT links
# git cherry-pick bd5150be56fcd312583d6cea2dc05f6de46694ca # contrib/lisp/ox-jabref.el: Misc. fixes
# git cherry-pick 01e3274d5a2e328c824bb9b45ff42e37b893d38f # lisp/ox-odt.el: Export to OpenDocument master file with `#+ODT_FILE_EXTENSION: odm'
# git cherry-pick 3b6a2df254135a69acdbe5662aa55dd6a9ad4cc6 # lisp/ox-odt.el: Allow customization of headline styles
# git cherry-pick 283c4683c74410553a257ca17e4d14a9a730f140 # lisp/ox-odt.el: Improve support for pagebreaks
# git cherry-pick 1a3a17ffcb853392fa86305f99c57c632e7da8ec # testing/examples/odt/pagebreak.org: New
# git cherry-pick cbc4d13d211bb866075b8030f0216c351acd7b81 # * testing/examples/odt/pagebreak.odt: New. Created from pagebreak.org.
# git cherry-pick c049488021621286b835af833ac0be551d9e4e7f # lisp/ox-odt.el (org-odt-transform-processes):  New macro "Update All and Break Links"
# git cherry-pick 73389e5eb4a7e09bc12efaa1f04aabe7da2ba6ac # * lisp/ox-odt.el (org-odt--read-attribute): Use `cl-case' instead of `case'
# git cherry-pick bece81cb3b31beed1441fc5c1bb9cdd641b11c1f # * lisp/ox-odt.el: Define *common* styles through #+ODT_EXTRA_STYLES: ...
# git cherry-pick dd0f812d99718cc8191b7cb5625461b82a9fb596 # * lisp/ox-odt.el: Simplify creation of tables with custom paragraph styles
# git cherry-pick 7cba22b1cf9e1c7a7aa7e55f3fc8cac81f84e247 # * contrib/lisp/ox-freemind.el: Use lexical binding
# git cherry-pick 51726e39376a4c2e0b75750b73ac99f6a979d06c # contrib/lisp/ox-freemind.el: Honor export options for todo, pri and tags
# git cherry-pick f95fa831a89aba237d03406c4be7e72049a42775 # * lisp/ox-odt.el: New in buffer option ODT_CONTENT_TEMPLATE_FILE
# git cherry-pick 5e4f6908d458e1422bf7d468b36807ee1206f31b # * lisp/ox-odt.el: Fix previous commit
# git cherry-pick b5475e18adcb28a1d2420a0b9acecdad34baae68 # ox-odt: Rationalize paragraph style names used within tables
# git cherry-pick 1093b829b91083a07692305d15941ead3c9911bc # * lisp/ox-odt.el (org-odt-table-cell--get-paragraph-styles): When a table specifies both a `:style' and `:p-style" attribute, only the table cells (i.e., cell borders etc) get styled as per the table template.  The paragraphs within the cells get styled *not* based on the template, but by "default" rules.
# git cherry-pick d791a043724f7d724d15692118777ef1fc7f4b93 # * lisp/ox-odt.el: New in buffer option ODT_DISPLAY_OUTLINE_LEVEL
# git cherry-pick bda0add75d0bf26a4fd0a0db02c3edd61a2f51ec # * lisp/ox-odt.el (org-odt-paragraph): Trim whitespace from paragraph contents
# git cherry-pick c18b736246bc110adea1933a56f0f94830decf6a # * etc/styles/OrgOdtStyles.xml(OrgOutline): New list style
# git cherry-pick 97ce8bd2b1ef3011a36ceecc67c4bb92ce9be9fe # * lisp/ox-odt.el (org-odt-headline): Minor Re-factoring
# git cherry-pick f8f75e806fb34c00eda4f20a3f2ae87c969e0f50 # * lisp/ox-odt.el (org-odt-headline): Minor refactoring
# git cherry-pick c1d2f132319aad1846c39215f2cce7387741aeb0 # * lisp/ox-odt.el (org-odt-headline): Minor refactoring
# git cherry-pick bf7b8c1aed5f6e4904da89dfe7b9301803d7018e # * lisp/ox-odt.el (org-odt-headline): Explicitly number headlines using a list style
# git cherry-pick 51b8dd673960e621ea9c2f77229c71de967b9eae # * lisp/ox-odt.el (org-odt-transform-processes): New macro `Reload'
# git cherry-pick 417081c96e7a28e581c69885123c8a0f132be5b8 # * lisp/ox-odt.el: Allow caption and name in list-tables
# git cherry-pick 4bffd4d5e2f1f334a09bf31456315ceac5b66977 # Merge changes from release_9.1.14
# git cherry-pick 53e8d26731aa32f8868e9de277965a858c75b14b # Now install ODT exporter from https://raw.githubusercontent.com/kjambunathan/org-mode-ox-odt/master/elpa/
# git cherry-pick a345448b4cc756f0df90f4326dc10d7c9237ffc6 # Fix previous commit
# git cherry-pick 765b8f933b85287d2c87218c5ebd35ff1b44e8de # * mk/odtpkg.mk: Fix earlier commit
# git cherry-pick e9f5129924931a5d4b559770e98a26626f39b6f6 # Publish ox-odt-9.1.14.145.tar
# git cherry-pick aaa2c292d0293b077a6e80112e510d9a768ba304 # * contrib/lisp/ox-jabref.el: Fix infinite recursion
# git cherry-pick c6cbd753d68e0bf47f84c9919745af3205b73a2e # mk/odtpkg.mk: Include OrgModeUtilities.oxt and net.sf.jabref.export.Chicago.ODF(English)-1.2.jar
# git cherry-pick 66c951c04dbe60cef919e090d1bfc39defe66868 # * contrib/lisp/ox-jabref.el: s/chicago.ODF/Chicago.ODF/
# git cherry-pick 8dc4cc74d355584af6591668d401b2ce5c45b63b # Publish ox-odt-9.1.14.149.tar
# git cherry-pick 47f047e08ca3bbedc7a7f30a3856975ab1f8b251 # Support for building `ox-odt' and `ox-jabref' packages
# git cherry-pick 38affaa2da5eab7ab8bbd2a1f192faddaebbbcc1 # Publish ox-odt-9.1.14.149.tar and ox-jabref-9.1.14.151.tar
# git cherry-pick 82f3c3a1924149b2d8942e5330ddfb7736f65610 # lisp/ox-odt.el, contrib/lisp/ox-jabref.el: Update library headers
# git cherry-pick ab90a1f5984047b54583b2e293b13af8c7a7d376 # mk/odtpg.mk: Re-implement using target-specific variables
# git cherry-pick f985d27abdf0b79efe8c4f36821ccc2af0c0960e # Publish ox-odt-9.1.14.154.tar and JabrefExportChicagoODF-1.2.0.tar
# git cherry-pick 945a85eff7fb60e30805282b6e817db7d2ef1460 # Publish ox-odt-9.1.14.155.tar & JabrefExportChicagoODF-1.2.0.tar
# git cherry-pick f8c51feb64df08772c16f02517df0658215ebcfb # notes/README.org: Renamed from README.org
# git cherry-pick 093be1983796f383d45fae4580e663ce402a7cd8 # notes/README.org: Add commits that are exclusive to this branch
# git cherry-pick 02f932327d87228a3fcc019457da250c7877b32d # notes/README.org: Note down features that are exclusive to this branch
# git cherry-pick ddab2a505c6e47a855af45ccd166fdb76bb18f12 # mk/odtpkg.mk: Ship './testing/examples/odt/' dir as `./samples/' dir in the ox-odt package
# git cherry-pick d0c9f6e2dba5e3e865a85cf8d651e37630175525 # Retire URL https://raw.githubusercontent.com/kjambunathan/org-mode-ox-odt/master/elpa/
# git cherry-pick 72476943406cc726d8dc6ea444c295b5164f638a # mk/odtpkg.el: Delete
# git cherry-pick bd37c925c76acb33b7d4072bae39e3e5bba50b71 # Use https://kjambunathan.github.io/elpa/ as the package URL for installing ox-odt and related packages
# git cherry-pick 7b1a84bfdd94c39a966dae446d137a6224b1ed36 # README.md: Update
# git cherry-pick e6dfbed4e8447a6e829cb3e1e0369580a17308b4 # * lisp/ox-odt.el (org-odt-transform-processes): Fix :type of this option
# git cherry-pick 9ba5d0c432af4ed2c5b91955d78a46e2d072a29f # Move OrgModeUtilities.oxt to contrib/odt/LibreOffice
# git cherry-pick 7d0d577e289a326407df21f5a8a5beceb4746834 # Add contrib/odt/LibreOffice/src/OrgModeUtilities.bas
# git cherry-pick dc1014fcbbb8e62b49025a5d33497fef878ad830 # OrgModeUtilities.bas (UpdateAll): Update Indices (NOT with `executeDispatch', but) with an API call
# git cherry-pick 09ea823c2e0f681968175c3ece5526d7f7cc7b44 # * contrib/odt/LibreOffice/OrgModeUtilities.oxt: Update
