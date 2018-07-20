# This script takes care of testing your crate

set -ex

# TODO This is the "test phase", tweak it as you see fit
main() {
    cross build --target $TARGET -- -C target-feature=$FEATURES
    cross build --target $TARGET --release -- -C target-feature=$FEATURES

    if [ ! -z $DISABLE_TESTS ]; then
        return
    fi

    cross test --target $TARGET -- -C target-feature=$FEATURES
    cross test --target $TARGET --release -- -C target-feature=$FEATURES

    # cross run --target $TARGET
    # cross run --target $TARGET --release
}

# we don't run the "test phase" when doing deploys
if [ -z $TRAVIS_TAG ]; then
    main
fi
