import type {ReactNode} from 'react';
import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  title: string;
  image: string;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: '使いやすさ',
    image: require('@site/static/img/logo.png').default,
    description: (
      <>
        Shol はシンプルな文法と直感的なルールで、初めての方でもその概念を理解しやすい言語です。
      </>
    ),
  },
  {
    title: '独自のパラダイム',
    image: require('@site/static/img/logo.png').default,
    description: (
      <>
        従来の手続き型やオブジェクト指向とは一線を画す、ルールベースの宣言型言語として、全く新しいプログラミング体験を提供します。
      </>
    ),
  },
  {
    title: 'Rust へのトランスパイル',
    image: require('@site/static/img/logo.png').default,
    description: (
      <>
        Shol のコンパイラは Rust のソースコードに変換できるため、Rust 対応の環境での実践利用も可能です。
      </>
    ),
  },
];

function Feature({title, image, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <img className={styles.featureSvg} role="img" src={image} />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
